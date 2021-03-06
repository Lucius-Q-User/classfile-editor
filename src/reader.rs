use super::{ClassVisitor, AnnotationVisitor, MethodVisitor, TypePath};
use super::{Type, TypeRef,  TypePathEntry, LocalVariableSpan, NameAndType};
use super::{ClassAccess, FieldAccess, MethodAccess, ParameterAccess, InnerClassAccess};
use super::{ModuleFlags, ExportFlags, RequireFlags};
use super::{AnnotationPrimitive, ClassConstant, ConstantDynamic, ClassVersion};
use super::{Label, Handle, FrameItem, FrameMode, opcodes};
use std::{char,
    collections::hash_map::{HashMap, Entry},
    rc::Rc,
    convert::TryInto,
    cell::RefCell
};
use bitflags::*;
use cesu8::Cesu8DecodingError;




bitflags! {
    pub struct ClassReaderFlags: u32 {
        const SKIP_CODE = 1;
        const SKIP_DEBUG = 2;
        const SKIP_FRAMES = 4;
    }
}
const SAME_LOCALS_1_STACK_ITEM_EXTENDED: u8 = 247;
const SAME_LOCALS_1_STACK_ITEM: u8 = 64;
const FRAME_RESERVED: u8 = 128;
const CHOP_FRAME: u8 = 248;
const SAME_FRAME_EXTENDED:u8 = 251;
const FULL_FRAME: u8 = 255;

#[derive(Debug, Copy, Clone)]
pub enum ClassDecodeError {
    UnexpectedEOF,
    InvalidMagic,
    UnknownVersion,
    ConstantPoolIndexOutOfBounds,
    ConstantPoolTypeMismatch,
    UnrecognizedConstantPoolEntry,
    UnrecognizedAnnotationTag,
    UnrecognizedTypePathKind,
    BytecodeTooLong,
    UnrecognizedInstruction,
    InvalidCodeOffset,
    UnrecognizedFrameType,
    UnrecognizedTypeRef,
    InvalidUtf8
}

impl From<Cesu8DecodingError> for ClassDecodeError {
    fn from(_: Cesu8DecodingError) -> ClassDecodeError {
        ClassDecodeError::InvalidUtf8
    }
}

pub type Result<T> = ::std::result::Result<T, ClassDecodeError>;

#[derive(Debug)]
pub struct ClassReader<'a> {
    this: ClassReaderW<'a>
}

#[derive(Debug)]
enum ClassReaderW<'a> {
    Reader(RefCell<ClassReaderInner<'a>>),
    Error(ClassDecodeError)
}

impl ClassReader<'_> {
    pub fn new(bytes: &[u8]) -> ClassReader {
        ClassReader {
            this: match ClassReaderInner::new(bytes) {
                Ok(this) => ClassReaderW::Reader(RefCell::new(this)),
                Err(err) => ClassReaderW::Error(err)
            }
        }
    }
    pub fn get_name(&self) -> Result<Rc<str>> {
        match &self.this {
            ClassReaderW::Reader(cell) => {
                cell.borrow_mut().get_name()
            },
            ClassReaderW::Error(err) => {
                Err(*err)
            }
        }
    }
    pub fn get_access(&self) -> Result<ClassAccess> {
        match &self.this {
            ClassReaderW::Reader(cell) => {
                cell.borrow_mut().get_access()
            },
            ClassReaderW::Error(err) => {
                Err(*err)
            }
        }
    }
    pub fn accept(&self, visitor: &mut dyn ClassVisitor, flags: ClassReaderFlags) -> Result<()> {
        match &self.this {
            ClassReaderW::Reader(cell) => {
                cell.borrow_mut().accept(visitor, flags)
            },
            ClassReaderW::Error(err) => {
                Err(*err)
            }
        }
    }
}

#[derive(Debug)]
struct ClassReaderInner<'a> {
    bytes: &'a [u8],
    const_pool: Vec<usize>,
    header_offset: usize,
    bootstrap_methods: Vec<usize>,
    string_cache: HashMap<usize, Rc<str>>
}


fn into_handle(cs: ClassConstant) -> Handle {
    if let ClassConstant::MethodHandle(hn) = cs {
        hn
    } else {
        panic!("");
    }
}

#[derive(Debug, Clone)]
struct TypeAnnotation {
    type_ref: TypeRef,
    type_path: TypePath,
    at: usize,
    visible: bool
}

impl TypeAnnotation {
    fn new(type_ref: TypeRef, type_path: TypePath, at: usize, visible: bool) -> TypeAnnotation {
        TypeAnnotation {
            type_path, type_ref, at, visible
        }
    }
}

type LocalVarAnnotation = (Vec<LocalVariableSpan>, TypeAnnotation);
type InsnAnnotations = HashMap<usize, Vec<TypeAnnotation>>;

impl<'a> ClassReaderInner<'a> {
    fn new<'b>(bytes: &'b [u8]) -> Result<ClassReaderInner<'b>> {
        let mut this = ClassReaderInner {
            bytes,
            const_pool: vec![],
            header_offset: 0,
            bootstrap_methods: Vec::new(),
            string_cache: HashMap::new()
        };
        let magic = this.read_u4(0)?;
        if magic != opcodes::CLASS_MAGIC {
            return Err(ClassDecodeError::InvalidMagic);
        }
        let version = this.read_u2(6)?;
        if version > opcodes::V14 {
            return Err(ClassDecodeError::UnknownVersion)
        }
        let const_pool_len = this.read_u2(8)? as usize;
        this.const_pool.reserve(const_pool_len);
        let mut offset = 10;
        this.const_pool.push(0); //Dummy zero-th entry
        let mut i = 1;
        while i < const_pool_len {
            this.const_pool.push(offset);
            match this.read_u1(offset)? {
                opcodes::CONSTANT_FIELDREF |
                opcodes::CONSTANT_METHODREF |
                opcodes::CONSTANT_INTERFACEMETHODREF |
                opcodes::CONSTANT_INTEGER |
                opcodes::CONSTANT_FLOAT |
                opcodes::CONSTANT_NAMEANDTYPE |
                opcodes::CONSTANT_INVOKEDYNAMIC |
                opcodes::CONSTANT_DYNAMIC => {
                    offset += 5;
                },
                opcodes::CONSTANT_LONG |
                opcodes::CONSTANT_DOUBLE => {
                    offset += 9;
                    this.const_pool.push(0);
                    i += 1;
                },
                opcodes::CONSTANT_METHODHANDLE => {
                    offset += 4;
                },
                opcodes::CONSTANT_CLASS |
                opcodes::CONSTANT_STRING |
                opcodes::CONSTANT_METHODTYPE |
                opcodes::CONSTANT_PACKAGE |
                opcodes::CONSTANT_MODULE => {
                    offset += 3;
                },
                opcodes::CONSTANT_UTF8 => {
                    let len = this.read_u2(offset + 1)? as usize;
                    offset += 3 + len;
                },
                _ => {
                    return Err(ClassDecodeError::UnrecognizedConstantPoolEntry);
                }
            }
            i += 1;
        }
        this.header_offset = offset;
        Ok(this)
    }
    fn read_u1(&self, idx: usize) -> Result<u8> {
        self.bytes.get(idx).cloned().ok_or(ClassDecodeError::UnexpectedEOF)
    }
    fn read_u2(&self, idx: usize) -> Result<u16> {
        let i = self.bytes.get(idx..idx + 2).ok_or(ClassDecodeError::UnexpectedEOF)?;
        Ok(u16::from_be_bytes(i.try_into().unwrap()))
    }
    fn read_u4(&self, idx: usize) -> Result<u32> {
        let i = self.bytes.get(idx..idx + 4).ok_or(ClassDecodeError::UnexpectedEOF)?;
        Ok(u32::from_be_bytes(i.try_into().unwrap()))
    }
    fn read_u8(&self, idx: usize) -> Result<u64> {
        let i = self.bytes.get(idx..idx + 8).ok_or(ClassDecodeError::UnexpectedEOF)?;
        Ok(u64::from_be_bytes(i.try_into().unwrap()))
    }
    fn read_f8(&self, idx: usize) -> Result<f64> {
        self.read_u8(idx).map(f64::from_bits)
    }
    fn read_i8(&self, idx: usize) -> Result<i64> {
        self.read_u8(idx).map(|c| c as i64)
    }
    fn read_i4(&self, idx: usize) -> Result<i32> {
        self.read_u4(idx).map(|c| c as i32)
    }
    fn read_i2(&self, idx: usize) -> Result<i16> {
        self.read_u2(idx).map(|c| c as i16)
    }
    fn read_i1(&self, idx: usize) -> Result<i8> {
        self.read_u1(idx).map(|c| c as i8)
    }
    fn read_f4(&self, idx: usize) -> Result<f32> {
        self.read_u4(idx).map(f32::from_bits)
    }
    fn read_stringlike(&mut self, idx: usize, kind: u8) -> Result<Option<Rc<str>>> {
        let pool_index = self.read_u2(idx)? as usize;
        if pool_index == 0 {
            return Ok(None);
        }
        let mut item_offset = self.const_pool.get(pool_index).cloned().ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        if self.read_u1(item_offset)? != kind {
            return Err(ClassDecodeError::ConstantPoolTypeMismatch);
        }
        item_offset += 1;
        self.read_utf8(item_offset).map(Some)
    }
    fn read_class_maybe(&mut self, idx: usize) -> Result<Option<Rc<str>>> {
        self.read_stringlike(idx, opcodes::CONSTANT_CLASS)
    }
    fn read_module(&mut self, idx: usize) -> Result<Rc<str>> {
        let md = self.read_stringlike(idx, opcodes::CONSTANT_MODULE)?;
        md.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_class(&mut self, idx:usize) -> Result<Rc<str>> {
        let cl = self.read_class_maybe(idx)?;
        cl.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_package(&mut self, idx:usize) -> Result<Rc<str>> {
        let pk = self.read_stringlike(idx, opcodes::CONSTANT_PACKAGE)?;
        pk.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_utf8(&mut self, idx: usize) -> Result<Rc<str>> {
        let utf = self.read_utf8_maybe(idx)?;
        utf.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_utf8_maybe(&mut self, idx: usize) -> Result<Option<Rc<str>>> {
        let pool_index = self.read_u2(idx)? as usize;
        if pool_index == 0 {
            return Ok(None);
        }
        let offset = self.const_pool.get(pool_index).cloned().ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        if let Entry::Occupied(val) = self.string_cache.entry(offset) {
            return Ok(Some(val.get().clone()));
        }
        if self.read_u1(offset)? != opcodes::CONSTANT_UTF8 {
            return Err(ClassDecodeError::ConstantPoolTypeMismatch)
        }
        let len = self.read_u2(offset + 1)? as usize;
        let strn = self.read_cesu8(offset + 3, len)?;
        Ok(Some(self.string_cache.entry(offset).or_insert_with(|| Rc::from(strn)).clone()))
    }
    fn read_cesu8(&self, data_offset: usize, data_size: usize) -> Result<String> {
        let slc = &self.bytes[data_offset..data_offset+data_size];
        Ok(cesu8::from_java_cesu8(slc)?.into_owned())
    }
    fn read_name_and_type_maybe(&mut self, at: usize) -> Result<Option<NameAndType>> {
        let item = self.read_u2(at)? as usize;
        if item != 0 {
            let &cpi = self.const_pool.get(item)
                .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
            if self.read_u1(cpi)? != opcodes::CONSTANT_NAMEANDTYPE {
                return Err(ClassDecodeError::ConstantPoolTypeMismatch)
            }
            let enclosing_name = self.read_utf8(cpi + 1)?;
            let enclosing_desc = self.read_utf8(cpi + 3)?;
            Ok(Some(NameAndType::new(enclosing_name, enclosing_desc)))
        } else {
            Ok(None)
        }
    }
    fn read_name_and_type(&mut self, at: usize) -> Result<NameAndType> {
        let rs = self.read_name_and_type_maybe(at)?;
        if let Some(r) = rs {
            Ok(r)
        } else {
            Err(ClassDecodeError::ConstantPoolIndexOutOfBounds)
        }
    }
    fn read_method_ref(&mut self, at: usize) -> Result<(Rc<str>, NameAndType, bool)> {
        let item = self.read_u2(at)? as usize;
        let cpi = *self.const_pool.get(item)
            .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let itf = self.read_u1(cpi)? == opcodes::CONSTANT_INTERFACEMETHODREF;
        let nat = self.read_name_and_type(cpi + 3)?;
        let owner = self.read_class(cpi + 1)?;
        Ok((owner, nat, itf))
    }
    fn get_name(&mut self) -> Result<Rc<str>> {
        let hdr = self.header_offset;
        Ok(self.read_class(hdr + 2)?)
    }
    fn get_access(&self) -> Result<ClassAccess> {
        let access_raw = self.read_u2(self.header_offset)?;
        Ok(ClassAccess::from_bits_truncate(access_raw.into()))
    }
    fn read_module_attr(&mut self, mut at: usize, module_packages: usize, 
                        module_main: Option<Rc<str>>, visitor: &mut dyn ClassVisitor) -> Result<()> {
        let name = self.read_module(at)?;
        let flags = ModuleFlags::from_bits_truncate(self.read_u2(at + 2)?);
        let version = self.read_utf8_maybe(at + 4)?;
        let mv = visitor.visit_module(name, flags, version);
        at += 6;
        if let Some(mv) = mv {
            if module_main.is_some() {
                mv.visit_main_class(module_main.unwrap());
            }
            if module_packages != 0 {
                let num = self.read_u2(module_packages - 2)? as usize;
                for i in 0..num {
                    let pkg = self.read_package(module_packages + i * 2)?;
                    mv.visit_package(pkg);
                }
            }
            let num_req = self.read_u2(at)?;
            at += 2;
            for _ in 0..num_req {
                let md = self.read_module(at)?;
                let flags = RequireFlags::from_bits_truncate(self.read_u2(at + 2)?);
                let version = self.read_utf8_maybe(at + 4)?;
                mv.visit_require(md, flags, version);
                at += 6;
            }
            let num_exp = self.read_u2(at)?;
            at += 2;
            for _ in 0..num_exp {
                let export = self.read_package(at)?;
                let access = ExportFlags::from_bits_truncate(self.read_u2(at + 2)?);
                let num_export_to = self.read_u2(at + 4)? as usize;
                at += 6;
                let mut export_to = Vec::with_capacity(num_export_to);
                for _ in 0..num_export_to {
                    let s = self.read_module(at)?;
                    export_to.push(s);
                    at += 2;
                }
                mv.visit_export(export, access, export_to);
            }
            let num_open = self.read_u2(at)?;
            at += 2;
            for _ in 0..num_open {
                let open = self.read_package(at)?;
                let access = ExportFlags::from_bits_truncate(self.read_u2(at + 2)?);
                let num_open_to = self.read_u2(at + 4)? as usize;
                at += 6;
                let mut open_to = Vec::with_capacity(num_open_to);
                for _ in 0..num_open_to {
                    let s = self.read_module(at)?;
                    open_to.push(s);
                    at += 2;
                }
                mv.visit_open(open, access, open_to);
            }
            let num_use = self.read_u2(at)?;
            at += 2;
            for _ in 0..num_use {
                let cl = self.read_class(at)?;
                at += 2;
                mv.visit_use(cl);
            }
            let num_provide = self.read_u2(at)?;
            at += 2;
            for _ in 0..num_provide {
                let service = self.read_class(at)?;
                let num_provide_with = self.read_u2(at + 2)? as usize;
                at += 4;
                let mut provide_with = Vec::with_capacity(num_provide_with);
                for _ in 0..num_provide_with {
                    let s = self.read_class(at)?;
                    provide_with.push(s);
                    at += 2;
                }
                mv.visit_provide(service, provide_with);
            }
            mv.visit_end();
        }
        Ok(())
    }
    fn accept(&mut self, visitor: &mut dyn ClassVisitor, flags: ClassReaderFlags) -> Result<()> {
        let mut hdr = self.header_offset;
        let version = ClassVersion::new(self.read_u2(6)?, self.read_u2(4)?);
        let mut access = self.get_access()?;
        let name = self.read_class(hdr + 2)?;
        let super_class = self.read_class_maybe(hdr + 4)?;
        let num_interfaces = self.read_u2(hdr + 6)?;
        let mut interfaces = Vec::new();
        hdr += 8;
        for _ in 0..num_interfaces {
            interfaces.push(self.read_class(hdr)?);
            hdr += 2;
        }
        hdr = self.get_attributes_offset()?;
        let mut source_file = None;
        let mut inner_classes = 0;
        let mut enclosing_class = None;
        let mut enclosing_method = None;
        let mut signature = None;
        let mut annotations_offset = 0;
        let mut type_annotations_offset = 0;
        let mut invisible_annotations_offset = 0;
        let mut invisible_type_annotations_offset = 0;
        let mut module = 0;
        let mut module_main = None;
        let mut module_packages = 0;
        let mut source_debug = None;
        let mut nest_host = None;
        let mut nest_members_offset = 0;
        let mut record_offset = 0;
        let num_attrs = self.read_u2(hdr)?;
        for _ in 0..num_attrs {
            let attr_name = &*self.read_utf8(hdr + 2)?;
            match attr_name {
                "SourceFile" => {
                    source_file = Some(self.read_utf8(hdr + 8)?)
                },
                "InnerClasses" => {
                    inner_classes = hdr + 8;
                },
                "EnclosingMethod" => {
                    enclosing_class = Some(self.read_class(hdr + 8)?);
                    enclosing_method = self.read_name_and_type_maybe(hdr + 10)?;
                },
                "Signature" => {
                    signature = Some(self.read_utf8(hdr + 8)?);
                },
                "RuntimeVisibleAnnotations" => {
                    annotations_offset = hdr + 8;
                },
                "RuntimeVisibleTypeAnnotations" => {
                    type_annotations_offset = hdr + 8;
                },
                "Deprecated" => {
                    access |= ClassAccess::ACC_PSEUDO_DEPRECATED;
                },
                "Synthetic" => {
                    access |= ClassAccess::ACC_SYNTHETIC;
                },
                "BootstrapMethods" => {
                    let num_meth = self.read_u2(hdr + 8)?;
                    self.bootstrap_methods.reserve(num_meth as usize);
                    let mut nat = hdr + 10;
                    for _ in 0..num_meth {
                        self.bootstrap_methods.push(nat);
                        nat += (4 + self.read_u2(nat + 2)? * 2) as usize;
                    }
                },
                "SourceDebugExtension" => {
                    let len = self.read_u4(hdr + 4)? as usize;
                    source_debug = Some(self.read_cesu8(hdr + 8, len)?);
                },
                "RuntimeInvisibleAnnotations" => {
                    invisible_annotations_offset = hdr + 8;
                },
                "RuntimeInvisibleTypeAnnotations" => {
                    invisible_type_annotations_offset = hdr + 8;
                },
                "NestHost" => {
                    nest_host = Some(self.read_class(hdr + 8)?);
                },
                "NestMembers" => {
                    nest_members_offset = hdr + 8;
                },
                "Module" => {
                    module = hdr + 8;
                },
                "ModuleMainClass" => {
                    module_main = Some(self.read_class(hdr + 8)?);
                },
                "ModulePackages" => {
                    module_packages = hdr + 10;
                },
                "Record" => {
                    record_offset = hdr + 8;
                },
                _ => {

                }
            }
            hdr += 6 + self.read_u4(hdr + 4)? as usize;
        }
        visitor.visit_header(version, access, name, signature, super_class, interfaces);
        if !flags.contains(ClassReaderFlags::SKIP_DEBUG) {
            visitor.visit_source(source_file, source_debug);
        }
        if module != 0 {
            self.read_module_attr(module, module_packages, module_main, visitor)?;
        }
        if nest_host.is_some() {
            visitor.visit_nest_host(nest_host.unwrap());
        }
        if enclosing_class.is_some() {
            visitor.visit_outer_class(enclosing_class.unwrap(), enclosing_method);
        }
        if annotations_offset != 0 {
            let num = self.read_u2(annotations_offset)?;
            let mut v = annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_annotation(desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis)?;
            }
        }
        if invisible_annotations_offset != 0 {
            let num = self.read_u2(invisible_annotations_offset)?;
            let mut v = invisible_annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_annotation(desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis)?;
            }
        }
        if type_annotations_offset != 0 {
            let num = self.read_u2(type_annotations_offset)?;
            let mut v = type_annotations_offset + 2;
            for _ in 0..num {
                let rt  = self.read_annotation_target(v)?;
                v = rt.0;
                let type_ref = rt.1;
                let type_path = rt.2;
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis)?
            }
        }
        if invisible_type_annotations_offset != 0 {
            let num = self.read_u2(invisible_type_annotations_offset)?;
            let mut v = invisible_type_annotations_offset + 2;
            for _ in 0..num {
                let rt  = self.read_annotation_target(v)?;
                v = rt.0;
                let type_ref = rt.1;
                let type_path = rt.2;
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis)?
            }
        }
        if nest_members_offset != 0 {
            let num = self.read_u2(nest_members_offset)?;
            let mut offset = nest_members_offset + 2;
            for _ in 0..num {
                let class = self.read_class(offset)?;
                visitor.visit_nest_member(class);
                offset += 2;
            }
        }
        if inner_classes != 0 {
            let mut offset = inner_classes + 2;
            let num = self.read_u2(inner_classes)?;
            for _ in 0..num {
                let inner = self.read_class(offset)?;
                let outer = self.read_class_maybe(offset + 2)?;
                let inner_name = self.read_utf8_maybe(offset + 4)?;
                let access = InnerClassAccess::from_bits_truncate(
                    self.read_u2(offset + 6)?);
                visitor.visit_inner_class(inner, outer, inner_name, access);
                offset += 8;
            }
        }
        let mut at = self.header_offset + 10 + 2 * (num_interfaces as usize);
        let nfields = self.read_u2(at - 2)?;
        for _ in 0..nfields {
            at = self.read_field(visitor, at)?;
        }
        let nmethods = self.read_u2(at)?;
        at += 2;
        for _ in 0..nmethods {
            at = self.read_method(visitor, at, flags)?;
        }
        if record_offset != 0 {
            let num = self.read_u2(record_offset)?;
            let mut offset = record_offset + 2;
            for _ in 0..num {
                offset = self.read_record_component(visitor, offset)?;
            }
        }
        visitor.visit_end();
        Ok(())
    }
    fn read_record_component(&mut self, vis: &mut dyn ClassVisitor, mut at: usize) -> Result<usize> {
        let name = self.read_utf8(at)?;
        let desc = self.read_utf8(at + 2)?;
        at += 2;
        let num_attrs = self.read_u2(at)?;
        let mut signature = None;
        let mut annotations_offset = 0;
        let mut invisible_annotations_offset = 0;
        let mut type_annotations_offset = 0;
        let mut invisible_type_annotations_offset = 0;
        for _ in 0..num_attrs {
            let attr_name = &*self.read_utf8(at + 2)?;
            match attr_name {
                "Signature" => {
                    signature = Some(self.read_utf8(at + 8)?);
                },
                "RuntimeVisibleAnnotations" => {
                    annotations_offset = at + 8;
                },
                "RuntimeVisibleTypeAnnotations" => {
                    type_annotations_offset = at + 8;
                },
                "RuntimeInvisibleAnnotations" => {
                    invisible_annotations_offset = at + 8;
                },
                "RuntimeInvisibleTypeAnnotations" => {
                    invisible_type_annotations_offset = at + 8;
                },
                _ => {

                }
            }
            at += 6 + self.read_u4(at + 4)? as usize;
        }
        at += 2;
        let rcv = vis.visit_record_component(NameAndType::new(name, desc), signature);
        if let Some(rcv) = rcv {
            if annotations_offset != 0 {
                let num = self.read_u2(annotations_offset)?;
                let mut v = annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v)?;
                    let mut vis = rcv.visit_annotation(desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?;
                }
            }
            if invisible_annotations_offset != 0 {
                let num = self.read_u2(invisible_annotations_offset)?;
                let mut v = invisible_annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v)?;
                    let mut vis = rcv.visit_annotation(desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?;
                }
            }
            if type_annotations_offset != 0 {
                let num = self.read_u2(type_annotations_offset)?;
                let mut v = type_annotations_offset + 2;
                for _ in 0..num {
                    let rt  = self.read_annotation_target(v)?;
                    v = rt.0;
                    let type_ref = rt.1;
                    let type_path = rt.2;
                    let desc = self.read_utf8(v)?;
                    let mut vis = rcv.visit_type_annotation(type_ref, type_path, desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?
                }
            }
            if invisible_type_annotations_offset != 0 {
                let num = self.read_u2(invisible_type_annotations_offset)?;
                let mut v = invisible_type_annotations_offset + 2;
                for _ in 0..num {
                    let rt  = self.read_annotation_target(v)?;
                    v = rt.0;
                    let type_ref = rt.1;
                    let type_path = rt.2;
                    let desc = self.read_utf8(v)?;
                    let mut vis = rcv.visit_type_annotation(type_ref, type_path, desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?
                }
            }
        }
        Ok(at)
    }
    fn read_field(&mut self, vis: &mut dyn ClassVisitor, mut at: usize) -> Result<usize> {
        let access_raw = self.read_u2(at)?.into();
        let mut access = FieldAccess::from_bits_truncate(access_raw);
        let name = self.read_utf8(at + 2)?;
        let desc = self.read_utf8(at + 4)?;
        at += 6;
        let num_attrs = self.read_u2(at)?;
        let mut signature = None;
        let mut annotations_offset = 0;
        let mut invisible_annotations_offset = 0;
        let mut type_annotations_offset = 0;
        let mut invisible_type_annotations_offset = 0;
        let mut value = None;
        for _ in 0..num_attrs {
            let attr_name = &*self.read_utf8(at + 2)?;
            match attr_name {
                "ConstantValue" => {
                    let itm = self.read_u2(at + 8)? as usize;
                    if itm != 0 {
                        value = Some(self.read_const(itm)?);
                    }
                },
                "Signature" => {
                    signature = Some(self.read_utf8(at + 8)?);
                },
                "RuntimeVisibleAnnotations" => {
                    annotations_offset = at + 8;
                },
                "RuntimeVisibleTypeAnnotations" => {
                    type_annotations_offset = at + 8;
                },
                "Deprecated" => {
                    access |= FieldAccess::ACC_PSEUDO_DEPRECATED;
                },
                "Synthetic" => {
                    access |= FieldAccess::ACC_SYNTHETIC;
                },
                "RuntimeInvisibleAnnotations" => {
                    invisible_annotations_offset = at + 8;
                },
                "RuntimeInvisibleTypeAnnotations" => {
                    invisible_type_annotations_offset = at + 8;
                },
                _ => {

                }
            }
            at += 6 + self.read_u4(at + 4)? as usize;
        }
        at += 2;
        let fv = vis.visit_field(access, NameAndType::new(name, desc), signature, value);
        if let Some(fv) = fv {
            if annotations_offset != 0 {
                let num = self.read_u2(annotations_offset)?;
                let mut v = annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v)?;
                    let mut vis = fv.visit_annotation(desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?;
                }
            }
            if invisible_annotations_offset != 0 {
                let num = self.read_u2(invisible_annotations_offset)?;
                let mut v = invisible_annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v)?;
                    let mut vis = fv.visit_annotation(desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?;
                }
            }
            if type_annotations_offset != 0 {
                let num = self.read_u2(type_annotations_offset)?;
                let mut v = type_annotations_offset + 2;
                for _ in 0..num {
                    let rt  = self.read_annotation_target(v)?;
                    v = rt.0;
                    let type_ref = rt.1;
                    let type_path = rt.2;
                    let desc = self.read_utf8(v)?;
                    let mut vis = fv.visit_type_annotation(type_ref, type_path, desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?
                }
            }
            if invisible_type_annotations_offset != 0 {
                let num = self.read_u2(invisible_type_annotations_offset)?;
                let mut v = invisible_type_annotations_offset + 2;
                for _ in 0..num {
                    let rt  = self.read_annotation_target(v)?;
                    v = rt.0;
                    let type_ref = rt.1;
                    let type_path = rt.2;
                    let desc = self.read_utf8(v)?;
                    let mut vis = fv.visit_type_annotation(type_ref, type_path, desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis)?
                }
            }
            fv.visit_end();
        }
        Ok(at)
    }
    fn read_method(&mut self, vis: &mut dyn ClassVisitor, mut at: usize, flags: ClassReaderFlags) -> Result<usize> {
        let access_raw = self.read_u2(at)?.into();
        let mut access = MethodAccess::from_bits_truncate(access_raw);
        let name = self.read_utf8(at + 2)?;
        let desc = self.read_utf8(at + 4)?;
        at += 6;
        let num_attrs = self.read_u2(at)?;
        let mut code = 0;
        let mut exceptions = Vec::new();
        let mut signature = None;
        let mut annotations_offset = 0;
        let mut invisible_annotations_offset = 0;
        let mut type_annotations_offset = 0;
        let mut invisible_type_annotations_offset = 0;
        let mut parameter_annotations_offset = 0;
        let mut invisible_parameter_annotations_offset = 0;
        let mut annotation_default = 0;
        let mut method_parameters = 0;
        for _ in 0..num_attrs {
            let attr_name = self.read_utf8(at + 2)?;
            match &*attr_name {
                "Code" if !flags.contains(ClassReaderFlags::SKIP_CODE) => {
                    code = at + 8;
                },
                "Exceptions" => {
                    let n_exceptions = self.read_u2(at + 8)?;
                    let mut exc_ref = at + 10;
                    for _ in 0..n_exceptions {
                        let name = self.read_class(exc_ref)?;
                        exceptions.push(name);
                        exc_ref += 2;
                    }
                },
                "Signature" => {
                    signature = Some(self.read_utf8(at + 8)?);
                },
                "Deprecated" => {
                    access |= MethodAccess::ACC_PSEUDO_DEPRECATED;
                },
                "RuntimeVisibleAnnotations" => {
                    annotations_offset = at + 8;
                },
                "RuntimeVisibleTypeAnnotations" => {
                    type_annotations_offset = at + 8;
                },
                "AnnotationDefault" => {
                    annotation_default = at + 8;
                }
                "Synthetic" => {
                    access |= MethodAccess::ACC_SYNTHETIC;
                },
                "RuntimeInvisibleAnnotations" => {
                    invisible_annotations_offset = at + 8;
                },
                "RuntimeInvisibleTypeAnnotations" => {
                    invisible_type_annotations_offset = at + 8;
                },
                "RuntimeVisibleParameterAnnotations" => {
                    parameter_annotations_offset = at + 8;
                },
                "RuntimeInvisibleParameterAnnotations" => {
                    invisible_parameter_annotations_offset = at + 8;
                },
                "MethodParameters" => {
                    method_parameters = at + 8;
                },
                _ => {

                }
            }
            at += 6 + self.read_u4(at + 4)? as usize;
        }
        at += 2;
        let visitor = if let Some(q) = vis.visit_method(access, NameAndType::new(name, desc), signature, exceptions) {
            q
        } else {
            return Ok(at);
        };
        if method_parameters != 0 {
            let count = self.read_u1(method_parameters)? as usize;
            for i in 0..count {
                let name = self.read_utf8(method_parameters + 1 + i * 4)?;
                let access_raw = self.read_u2(method_parameters + 3 + i * 4)?;
                let access = ParameterAccess::from_bits_truncate(access_raw);
                visitor.visit_parameter(name, access);
            }
        }
        if annotation_default != 0 {
            let mut vis = visitor.visit_annotation_default();
            self.read_annotation_value(annotation_default, Rc::from(""), &mut vis)?;
            if let Some(v) = vis {
                v.visit_end()
            }
        }
        if annotations_offset != 0 {
            let num = self.read_u2(annotations_offset)?;
            let mut v = annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_annotation(desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis)?;
            }
        }
        if invisible_annotations_offset != 0 {
            let num = self.read_u2(invisible_annotations_offset)?;
            let mut v = invisible_annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_annotation(desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis)?;
            }
        }
        if type_annotations_offset != 0 {
            let num = self.read_u2(type_annotations_offset)?;
            let mut v = type_annotations_offset + 2;
            for _ in 0..num {
                let rt  = self.read_annotation_target(v)?;
                v = rt.0;
                let type_ref = rt.1;
                let type_path = rt.2;
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis)?
            }
        }
        if invisible_type_annotations_offset != 0 {
            let num = self.read_u2(invisible_type_annotations_offset)?;
            let mut v = invisible_type_annotations_offset + 2;
            for _ in 0..num {
                let rt  = self.read_annotation_target(v)?;
                v = rt.0;
                let type_ref = rt.1;
                let type_path = rt.2;
                let desc = self.read_utf8(v)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis)?
            }
        }
        if parameter_annotations_offset != 0 {
            let mut at = parameter_annotations_offset;
            let pars = self.read_u1(at)?;
            visitor.visit_annotable_parameter_count(pars, true);
            at += 1;
            for i in 0..pars {
                let num = self.read_u2(at)?;
                at += 2;
                for _ in 0..num {
                    let desc = self.read_utf8(at)?;
                    let mut vis = visitor.visit_parameter_annotation(i, desc, true);
                    at = self.read_annotation_values(at + 2, true, &mut vis)?
                }
            }
        }
        if invisible_parameter_annotations_offset != 0 {
            let mut at = invisible_parameter_annotations_offset;
            let pars = self.read_u1(at)?;
            visitor.visit_annotable_parameter_count(pars, false);
            at += 1;
            for i in 0..pars {
                let num = self.read_u2(at)?;
                at += 2;
                for _ in 0..num {
                    let desc = self.read_utf8(at)?;
                    let mut vis = visitor.visit_parameter_annotation(i, desc, false);
                    at = self.read_annotation_values(at + 2, true, &mut vis)?
                }
            }
        }
        if code == 0 {
            visitor.visit_end();
            return Ok(at);
        }
        visitor.visit_code();
        self.read_code(visitor, code, flags)?;
        visitor.visit_end();
        Ok(at)
    }
    fn read_type_annotations(&mut self, vis: &mut dyn MethodVisitor, mut at: usize, visible: bool,
                             labels: &mut [Option<Label>]) -> Result<(Vec<LocalVarAnnotation>, InsnAnnotations)> {
        let n_annot = self.read_u2(at)?;
        let mut local_var_annot = Vec::new();
        let mut insn_annot = HashMap::new();
        at += 2;
        for _ in 0..n_annot {
            let target = self.read_u1(at)?;
            at += 1;
            match target {
                0x40 | 0x41 => {
                    let table_len = self.read_u2(at)? as usize;
                    let mut ranges = Vec::with_capacity(table_len);
                    at += 2;
                    for _ in 0..table_len {
                        let start_offset = self.read_u2(at)? as isize;
                        let start = create_label(start_offset, labels)?;
                        let end = create_label(start_offset + self.read_u2(at + 2)? as isize, labels)?;
                        let var = self.read_u2(at + 4)?;
                        at += 6;
                        ranges.push(LocalVariableSpan::new(start, end, var));
                    }
                    let tph = self.read_type_path(at)?;
                    at = tph.0;
                    let type_path = tph.1;
                    let kind = if target == 0x40 { TypeRef::LocalVariable } else { TypeRef::ResourceVariable };
                    let annot = TypeAnnotation::new(kind, type_path, at, visible);
                    local_var_annot.push((ranges, annot));
                    at = self.read_annotation_values(at + 2, true, &mut None)?;
                },
                0x42 => {
                    let catch_index = TypeRef::ThrowsClause(self.read_u2(at)?);
                    at += 2;
                    let tph = self.read_type_path(at)?;
                    at = tph.0;
                    let type_path = tph.1;
                    let desc = self.read_utf8(at)?;
                    at += 2;
                    let mut av = vis.visit_trycatch_annotation(catch_index, type_path, desc, visible);
                    at = self.read_annotation_values(at, true, &mut av)?;
                },
                0x43 | 0x44 | 0x45 | 0x46 => {
                    let insn_offset = self.read_u2(at)? as usize;
                    at += 2;
                    let tph = self.read_type_path(at)?;
                    at = tph.0;
                    let type_path = tph.1;
                    let kind = match target {
                        0x43 => TypeRef::Instanceof,
                        0x44 => TypeRef::New,
                        0x45 => TypeRef::ContructorReference,
                        0x46 => TypeRef::MethodReference,
                        _ => unreachable!()
                    };
                    insn_annot.entry(insn_offset).or_insert_with(Vec::new)
                              .push(TypeAnnotation::new(kind, type_path, at, visible));
                    at = self.read_annotation_values(at + 2, true, &mut None)?;
                },
                0x47 | 0x48 | 0x49 | 0x4A | 0x4B => {
                    let insn_offset = self.read_u2(at)? as usize;
                    at += 2;
                    let type_arg = self.read_u1(at)?;
                    at += 1;
                    let tph = self.read_type_path(at)?;
                    at = tph.0;
                    let type_path = tph.1;
                    let kind = match target {
                        0x47 => TypeRef::Cast(type_arg),
                        0x48 => TypeRef::GenericConstructorInvocation(type_arg),
                        0x49 => TypeRef::GenericMethodInvocation(type_arg),
                        0x4A => TypeRef::GenericConstructorReference(type_arg),
                        0x4B => TypeRef::GenericMethodReference(type_arg),
                        _ => unreachable!()
                    };
                    insn_annot.entry(insn_offset).or_insert_with(Vec::new)
                              .push(TypeAnnotation::new(kind, type_path, at, visible));
                    at = self.read_annotation_values(at + 2, true, &mut None)?;
                },
                _ => {
                    return Err(ClassDecodeError::UnrecognizedTypeRef);
                }
            }
        }
        Ok((local_var_annot, insn_annot))
    }
    fn precreate_labels(&self, code_begin: usize, code_length: usize) -> Result<Vec<Option<Label>>> {
        let code_end = code_begin + code_length;
        let mut labels = vec![None; code_length + 2];
        let mut at = code_begin;
        create_label(code_length as isize + 1, &mut labels)?;
        while at < code_end {
            let offset = (at - code_begin) as isize;
            let opcode = self.read_u1(at)?;
            match classify_insn(opcode)? {
                InsnClass::NoArg |
                InsnClass::ImplVar => {
                    at += 1;
                },
                InsnClass::Label => {
                    let lbl = offset + self.read_i2(at + 1)? as isize;
                    create_label(lbl, &mut labels)?;
                    at += 3;
                },
                InsnClass::WideLabel => {
                    let lbl = offset + self.read_i4(at + 1)? as isize;
                    create_label(lbl, &mut labels)?;
                    at += 5;
                },
                InsnClass::Wide => {
                    let opcode = self.read_u1(at + 1)?;
                    if opcode == opcodes::IINC {
                        at += 6;
                    } else {
                        at += 4;
                    }
                },
                InsnClass::Tableswitch => {
                    at = at + 4 - (offset & 3) as usize;
                    create_label(offset + self.read_i4(at)? as isize, &mut labels)?;
                    let entries = self.read_u4(at + 8)? - self.read_u4(at + 4)? + 1;
                    for _ in 0..entries {
                        create_label(offset + self.read_i4(at + 12)? as isize, &mut labels)?;
                        at += 4;
                    }
                    at += 12;
                },
                InsnClass::Lookupswitch => {
                    at = at + 4 - (offset & 3) as usize;
                    create_label(offset + self.read_i4(at)? as isize, &mut labels)?;
                    for _ in 0..self.read_u4(at + 4)? {
                        create_label(offset + self.read_i4(at + 12)? as isize, &mut labels)?;
                        at += 8;
                    }
                    at += 8;
                },
                InsnClass::Var |
                InsnClass::Sbyte |
                InsnClass::Ldc => {
                    at += 2;
                },
                InsnClass::Short |
                InsnClass::LdcWide |
                InsnClass::FieldMeth |
                InsnClass::Type |
                InsnClass::Iinc => {
                    at += 3;
                },
                InsnClass::ItfMeth |
                InsnClass::IndyMeth => {
                    at += 5;
                },
                InsnClass::Multianew => {
                    at += 4;
                }
            }
        }
        Ok(labels)
    }
    fn read_code(&mut self, vis: &mut dyn MethodVisitor, mut at: usize, flags: ClassReaderFlags) -> Result<()> {
        let max_stack = self.read_u2(at)?;
        let max_local = self.read_u2(at + 2)?;
        let code_length = self.read_u4(at + 4)? as usize;
        if code_length >= 65536 {
            return Err(ClassDecodeError::BytecodeTooLong);
        }
        let code_begin = at + 8;
        let code_end = code_begin + code_length;
        let mut labels = self.precreate_labels(code_begin, code_end)?;
        at = code_end;
        let num_try_catch = self.read_u2(at)?;
        for _ in 0..num_try_catch {
            let start = create_label(self.read_u2(at + 2)? as isize, &mut labels)?;
            let end = create_label(self.read_u2(at + 4)? as isize, &mut labels)?;
            let handler = create_label(self.read_u2(at + 6)? as isize, &mut labels)?;
            let class = self.read_class_maybe(at + 8)?;
            vis.visit_try_catch(start, end, handler, class);
            at += 8;
        }
        at += 2;
        let nattr = self.read_u2(at)?;
        let mut variables_table = 0;
        let mut variables_type_table = 0;
        let mut line_numbers = HashMap::new();
        let mut stack_map = 0;
        let mut frame_count = 0;
        let mut local_var_annot = Vec::new();
        let mut insn_annot = HashMap::new();
        for _ in 0..nattr {
            let name = self.read_utf8(at + 2)?;
            match &*name {
                "LocalVariableTable" if !flags.contains(ClassReaderFlags::SKIP_DEBUG) => {
                    variables_table = at + 8;
                    let nvars = self.read_u2(at + 8)?;
                    let mut nat = at;
                    for _ in 0..nvars {
                        let mut addr = self.read_u2(nat + 10)?;
                        create_label(addr as isize, &mut labels)?;
                        addr += self.read_u2(nat + 12)?;
                        create_label(addr as isize, &mut labels)?;
                        nat += 10;
                    }
                },
                "LocalVariableTypeTable" => {
                    variables_type_table = at + 8;
                },
                "LineNumberTable" if !flags.contains(ClassReaderFlags::SKIP_DEBUG) => {
                    let table_len = self.read_u2(at + 8)?;
                    let mut nat = at;
                    for _ in 0..table_len {
                        let addr = self.read_u2(nat + 10)?;
                        create_label(addr as isize, &mut labels)?;
                        let lb = labels[addr as usize].unwrap();
                        let line = self.read_u2(nat + 12)?;
                        line_numbers.entry(lb).or_insert_with(Vec::new).push(line);
                        nat += 4;
                    }
                },
                "RuntimeVisibleTypeAnnotations" | "RuntimeInvisibleTypeAnnotations" => {
                    let visible = &*name == "RuntimeVisibleTypeAnnotations";
                    let r = self.read_type_annotations(vis, at + 8, visible, &mut labels)?;
                    local_var_annot = r.0;
                    insn_annot = r.1;
                },
                "StackMapTable" if !flags.contains(ClassReaderFlags::SKIP_FRAMES) => {
                    stack_map = at + 10;
                    let stack_map_size = self.read_u4(at + 4)? as usize;
                    frame_count = self.read_u2(at + 8)?;
                    for i in stack_map..(stack_map + stack_map_size - 2) {
                        if self.read_u1(i)? == 8 {
                           let v = self.read_u2(i + 1)? as usize;
                            if v < code_length && self.read_u1(code_begin + v)? == opcodes::NEW {
                                create_label(v as isize, &mut labels)?;
                            }
                        }
                    }
                },
                _ => {

                }
            }
            at += 6 + self.read_u4(at + 4)? as usize;
        }
        let mut frame_offset = usize::max_value();
        let mut frame_mode = FrameMode::Full;
        let mut local_count = 0;
        let mut stack_count = 0;
        let mut locals = vec![FrameItem::Void; max_local as usize];
        let mut stack = vec![FrameItem::Void; max_stack as usize];
        let mut ran_out_of_frames = false;
        at = code_begin;
        while at < code_end {
            let offset = at - code_begin;
            if let Some(l) = labels[offset] {
                vis.visit_label(l);
                for line in line_numbers.remove(&l).unwrap_or_default() {
                    vis.visit_line_number(line, l);
                }
            }

            while !ran_out_of_frames &&
                (frame_offset == offset || frame_offset == usize::max_value()) {
                    if frame_offset != usize::max_value() {
                        vis.visit_frame(frame_mode, &locals[0..local_count as usize], &stack[0..stack_count as usize]);
                    }
                    if frame_count > 0 {
                        let tag = self.read_u1(stack_map)?;
                        let delta;
                        stack_map += 1;
                        if tag < SAME_LOCALS_1_STACK_ITEM {
                            delta = tag as usize;
                            frame_mode = FrameMode::Same;
                            stack_count = 0;
                        } else if tag < FRAME_RESERVED {
                            delta = (tag - SAME_LOCALS_1_STACK_ITEM) as usize;
                            frame_mode = FrameMode::Same1;
                            stack_count = 1;
                            stack_map = self.read_frame_type(&mut stack, 0, stack_map, &labels)?;
                        } else {
                            delta = self.read_u2(stack_map)? as usize;
                            stack_map += 2;
                            if tag == SAME_LOCALS_1_STACK_ITEM_EXTENDED {
                                stack_map = self.read_frame_type(&mut stack, 0, stack_map, &labels)?;
                                stack_count = 1;
                                frame_mode = FrameMode::Same1;
                            } else if tag >= CHOP_FRAME && tag < SAME_FRAME_EXTENDED {
                                frame_mode = FrameMode::Chop;
                                local_count -= u16::from(SAME_FRAME_EXTENDED - tag);
                                stack_count = 0;
                            } else if tag == SAME_FRAME_EXTENDED {
                                stack_count = 0;
                                frame_mode = FrameMode::Same;
                            } else if tag < FULL_FRAME {
                                for i in 0..(tag - SAME_FRAME_EXTENDED) {
                                    stack_map = self.read_frame_type(&mut locals, i as usize, stack_map, &labels)?;
                                }
                                frame_mode = FrameMode::Append;
                                local_count += u16::from(tag - SAME_FRAME_EXTENDED);
                                stack_count = 0;
                            } else {
                                frame_mode = FrameMode::Full;
                                local_count = self.read_u2(stack_map)?;
                                stack_map += 2;
                                for i in 0..local_count {
                                    stack_map = self.read_frame_type(&mut locals, i as usize, stack_map, &labels)?;
                                }
                                stack_count = self.read_u2(stack_map)?;
                                stack_map += 2;
                                for i in 0..stack_count {
                                    stack_map = self.read_frame_type(&mut stack, i as usize, stack_map, &labels)?;
                                }
                            }
                        }
                        frame_offset = frame_offset.wrapping_add(delta + 1);
                        frame_count -= 1;
                    } else {
                        ran_out_of_frames = true;
                    }
                }

            at = self.read_insn(at, vis, &labels, offset)?;

            for annot in insn_annot.remove(&offset).unwrap_or_default().into_iter() {
                let desc = self.read_utf8(annot.at)?;
                let mut av = vis.visit_insn_annotation(annot.type_ref, annot.type_path, desc, annot.visible);
                self.read_annotation_values(annot.at + 2, true, &mut av)?;
            }
        }
        if let Some(label) = labels[code_length] {
            vis.visit_label(label);
        }
        self.read_variables_table(variables_table, variables_type_table, vis, &labels)?;

        for (ranges, annot) in local_var_annot {
            let name = self.read_utf8(annot.at)?;
            let mut av = vis.visit_local_variable_annotation(annot.type_ref, annot.type_path, ranges, name, annot.visible);
            self.read_annotation_values(annot.at + 2, true, &mut av)?;
        }
        vis.visit_maxs(max_stack, max_local);
        Ok(())
    }
    fn read_variables_table(&mut self, var_table: usize, var_type_table: usize, vis: &mut dyn MethodVisitor,
                            labels: &[Option<Label>]) -> Result<()> {
        if var_table == 0 {
            return Ok(());
        }
        let mut type_table = None;
        let mut tt_len = 0;
        if var_type_table != 0 {
            let mut at = var_type_table + 2;
            tt_len = (self.read_u2(var_type_table)? * 3) as usize;
            let mut tt = Vec::with_capacity(tt_len as usize);
            for _ in 0..(tt_len / 3) {
                tt.push(at + 6);
                tt.push(self.read_u2(at + 8)? as usize);
                tt.push(self.read_u2(at)? as usize);
                at += 10;
            }
            type_table = Some(tt);
        }
        let mut at = var_table + 2;
        for _ in 0..self.read_u2(var_table)? {
            let start = self.read_u2(at)? as usize;
            let len = self.read_u2(at + 2)? as usize;
            let idx = self.read_u2(at + 8)?;
            let mut sig = None;
            if let Some(ref tt) = type_table {
                for i in (0..tt_len).step_by(3) {
                    if tt[i] == start && tt[i + 1] == idx as usize {
                        sig = Some(self.read_utf8(tt[i + 2])?);
                    }
                }
            }
            let name = self.read_utf8(at + 4)?;
            let desc = self.read_utf8(at + 6)?;
            let span = LocalVariableSpan::new(labels[start].unwrap(), labels[start + len].unwrap(), idx);
            vis.visit_local_var(NameAndType::new(name, desc), sig, span);
            at += 10;
        }
        Ok(())
    }
    fn read_insn(&mut self, mut at: usize, vis: &mut dyn MethodVisitor, labels: &[Option<Label>],
                 offset: usize) -> Result<usize> {
        let opcode = self.read_u1(at)?;
        match classify_insn(opcode)? {
            InsnClass::NoArg => {
                vis.visit_insn(opcode);
                Ok(at + 1)
            },
            InsnClass::ImplVar => {
                if opcode > opcodes::ISTORE {
                    let opcode_real = opcodes::ISTORE + ((opcode - 59) >> 2);
                    // ^ISTORE_0
                    vis.visit_var_insn(opcode_real, ((opcode - 59) & 0x3).into());
                } else {
                    let opcode_real = opcodes::ILOAD + ((opcode - 26) >> 2);
                    // ^ILOAD_0
                    vis.visit_var_insn(opcode_real, ((opcode - 26) & 0x3).into());
                }
                Ok(at + 1)
            },
            InsnClass::Label => {
                let os = offset as isize + self.read_i2(at + 1)? as isize;
                let target = labels[os as usize];
                vis.visit_jump_insn(opcode, target.unwrap());
                Ok(at + 3)
            },
            InsnClass::WideLabel => {
                let os = offset as isize + self.read_i4(at + 1)? as isize;
                let target = labels[os as usize];
                vis.visit_jump_insn(opcode, target.unwrap());
                Ok(at + 5)
            },
            InsnClass::Wide => {
                let opcode = self.read_u1(at + 1)?;
                if opcode == opcodes::IINC {
                    let var = self.read_u2(at + 2)?;
                    let by = self.read_i2(at + 4)?;
                    vis.visit_iinc_insn(var, by);
                    Ok(at + 6)
                } else {
                    vis.visit_var_insn(opcode, self.read_u2(at + 2)?);
                    Ok(at + 4)
                }
            },
            InsnClass::Tableswitch => {
                at = at + 4 - (offset & 3) as usize;
                let dflt_target = offset as isize + self.read_i4(at)? as isize;
                let dflt = labels[dflt_target as usize].unwrap();
                let min = self.read_i4(at + 4)?;
                let max = self.read_i4(at + 8)?;
                at += 12;
                let entries = (max - min + 1) as usize;
                let mut targets = Vec::with_capacity(entries);
                for _ in 0..entries {
                    let target = offset as isize + self.read_i4(at)? as isize;
                    targets.push(labels[target as usize].unwrap());
                    at += 4;
                }
                vis.visit_tableswitch_insn(min, max, dflt, targets);
                Ok(at)
            },
            InsnClass::Lookupswitch => {
                at = at + 4 - (offset & 3) as usize;
                let dflt_target = offset as isize + self.read_i4(at)? as isize;
                let dflt = labels[dflt_target as usize].unwrap();
                let entries = self.read_u4(at + 4)? as usize;
                let mut pairs = Vec::with_capacity(entries);
                at += 8;
                for _ in 0..entries {
                    let k = self.read_i4(at)?;
                    let t = self.read_i4(at + 4)? as isize + offset as isize;
                    let v = labels[t as usize].unwrap();
                    pairs.push((k, v));
                    at += 8;
                }
                vis.visit_lookupswitch_insn(dflt, pairs);
                Ok(at)
            },
            InsnClass::Var => {
                vis.visit_var_insn(opcode, self.read_u1(at + 1)?.into());
                Ok(at + 2)
            },
            InsnClass::Sbyte => {
                let operand = self.read_i1(at + 1)?.into();
                vis.visit_int_insn(opcode, operand);
                Ok(at + 2)
            },
            InsnClass::Ldc => {
                let idx = self.read_u1(at + 1)?;
                let cst = self.read_const(idx as usize)?;
                vis.visit_ldc_insn(cst);
                Ok(at + 2)
            },
            InsnClass::Short => {
                let operand = self.read_i2(at + 1)?.into();
                vis.visit_int_insn(opcode, operand);
                Ok(at + 3)
            },
            InsnClass::Iinc => {
                let var = self.read_u1(at + 1)?.into();
                let by = self.read_i1(at + 2)?.into();
                vis.visit_iinc_insn(var, by);
                Ok(at + 3)
            },
            InsnClass::LdcWide => {
                let idx = self.read_u2(at + 1)?;
                let cst = self.read_const(idx as usize)?;
                vis.visit_ldc_insn(cst);
                Ok(at + 3)
            },
            InsnClass::ItfMeth |
            InsnClass::FieldMeth => {
                let mdata = self.read_method_ref(at + 1)?;
                vis.visit_field_method_insn(opcode, mdata.0, mdata.1);
                if opcode == opcodes::INVOKEINTERFACE {
                    Ok(at + 5)
                } else {
                    Ok(at + 3)
                }
            },
            InsnClass::IndyMeth => {
                let dta = self.read_indy(at + 1)?;
                vis.visit_indy_insn(dta.method, dta.bootstrap, dta.args);
                Ok(at + 5)
            },
            InsnClass::Type => {
                let cls = self.read_class(at + 1)?;
                vis.visit_type_insn(opcode, cls);
                Ok(at + 3)
            },
            InsnClass::Multianew => {
                let class = self.read_class(at + 1)?;
                let dim = self.read_u1(at + 3)?;
                vis.visit_multianew_insn(class, dim);
                Ok(at + 4)
            }
        }
    }
    fn read_frame_type(&mut self, frame: &mut [FrameItem], idx: usize, mut at: usize, labels: &[Option<Label>],
                       ) -> Result<usize> {
        let ty = self.read_u1(at)?;
        at += 1;
        frame[idx] = match ty {
            0 => FrameItem::Top,
            1 => FrameItem::Integer,
            2 => FrameItem::Float,
            3 => FrameItem::Double,
            4 => FrameItem::Long,
            5 => FrameItem::Null,
            6 => FrameItem::UninitializedThis,
            7 => {
                let cs = self.read_class(at)?;
                at += 2;
                FrameItem::Class(cs)
            },
            8 => {
                let lq = labels[self.read_u2(at)? as usize];
                at += 2;
                FrameItem::Uninitialized(lq.unwrap())
            },
            _ => {
                return Err(ClassDecodeError::UnrecognizedFrameType);
            }
        };
        Ok(at)
    }
    fn read_indy(&mut self, at: usize) -> Result<ConstantDynamic> {
        let cpi = *self.const_pool.get(self.read_u2(at)? as usize)
            .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let mut bootstrap_index = self.bootstrap_methods[self.read_u2(cpi + 1)? as usize];
        let method = self.read_name_and_type(cpi + 3)?;
        let bootstrap = into_handle(self.read_const(self.read_u2(bootstrap_index)? as usize)?);
        let bsm_argc = self.read_u2(bootstrap_index + 2)? as usize;
        let mut bsm_argv = Vec::with_capacity(bsm_argc);
        bootstrap_index += 4;
        for _ in 0..bsm_argc {
            bsm_argv.push(self.read_const(self.read_u2(bootstrap_index)? as usize)?);
            bootstrap_index += 2;
        }
        Ok(ConstantDynamic {bootstrap, method, args: bsm_argv})
    }
    fn read_annotation_values(&mut self, mut at: usize, named: bool, mut vis: &mut Option<&mut dyn AnnotationVisitor>) 
                              -> Result<usize> {
        let num = self.read_u2(at)?;
        at += 2;
        if named {
            for _ in 0..num {
                let name = self.read_utf8(at)?;
                at = self.read_annotation_value(at + 2, name, &mut vis)?;
            }
        } else {
            for _ in 0..num {
                at = self.read_annotation_value(at, Rc::from(""), &mut vis)?;
            }
        }
        if let Some(v) = vis {
            v.visit_end();
        }
        Ok(at)
    }
    fn read_annotation_value(&mut self, mut at: usize, name: Rc<str>, vis: &mut Option<&mut dyn AnnotationVisitor>) 
                             -> Result<usize> {
        let tag = self.read_u1(at)?;
        if vis.is_none() {
            match tag as char {
                'e' => {
                    return Ok(at + 5);
                },
                '@' => {
                    return self.read_annotation_values(at + 3, true, vis);
                },
                '[' => {
                    return self.read_annotation_values(at + 1, false, vis);
                },
                _ => {
                    return Ok(at + 3);
                }
            }
        } else if let Some(vis) = vis {
            at += 1;
            match tag as char {
                'I' | 'F' | 'D' | 'J' | 'B' | 'C' | 'S' | 'Z' => {
                    let csta = self.read_u2(at)? as usize;
                    let cst = self.read_const(csta)?;
                    //TODO: learn how macros work and remove duplication here
                    match tag as char {
                        'I' => {
                            if let ClassConstant::Integer(c) = cst {
                                vis.visit_primitive(name, AnnotationPrimitive::Integer(c));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'F' => {
                            if let ClassConstant::Float(c) = cst {
                                vis.visit_primitive(name, AnnotationPrimitive::Float(c));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'D' => {
                            if let ClassConstant::Double(c) = cst {
                                vis.visit_primitive(name, AnnotationPrimitive::Double(c));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'J' => {
                            if let ClassConstant::Long(c) = cst {
                                vis.visit_primitive(name, AnnotationPrimitive::Long(c));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'B' => {
                            if let ClassConstant::Integer(c) = cst {
                                vis.visit_primitive(name,
                                                    AnnotationPrimitive::Byte(c as i8));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'C' => {
                            if let ClassConstant::Integer(c) = cst {
                                vis.visit_primitive(name,
                                                    AnnotationPrimitive::Character(c as i16));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'S' => {
                            if let ClassConstant::Integer(c) = cst {
                                vis.visit_primitive(name,
                                                    AnnotationPrimitive::Short(c as i16));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        'Z' => {
                            if let ClassConstant::Integer(c) = cst {
                                vis.visit_primitive(name,
                                                    AnnotationPrimitive::Boolean(c != 0));
                            } else {
                                return Err(ClassDecodeError::ConstantPoolTypeMismatch);
                            }
                        },
                        _ => {
                            unreachable!();
                        }
                    }
                    return Ok(at + 2);
                },
                's' => {
                    let string = self.read_utf8(at)?;
                    vis.visit_primitive(name,
                                        AnnotationPrimitive::String(string));
                    return Ok(at + 2);
                },
                'e' => {
                    let value = self.read_utf8(at + 2)?;
                    let desc = self.read_utf8(at)?;
                    vis.visit_enum(name, desc, value);
                    return Ok(at + 4);
                },
                'c' => {
                    let desc = self.read_utf8(at)?;
                    vis.visit_primitive(name,
                                        AnnotationPrimitive::Type(Type::new(desc)));
                    return Ok(at + 2);
                }
                '@' => {
                    let desc = self.read_utf8(at)?;
                    let mut nv = vis.visit_annotation(name, desc);
                    at = self.read_annotation_values(at + 2, true, &mut nv)?;
                },
                '[' => {
                    let mut nv = vis.visit_array(name);
                    return self.read_annotation_values(at, false, &mut nv);
                },
                _ => {
                    return Err(ClassDecodeError::UnrecognizedAnnotationTag);
                }
            }
            return Ok(at);

        }
        unreachable!();
    }
    fn read_annotation_target(&self, mut at: usize) -> Result<(usize, TypeRef, TypePath)> {
        let target = self.read_u1(at)?;
        let typeref;
        match target {
            0x0 => {
                let idx = self.read_u1(at + 1)?;
                typeref = TypeRef::ClassTypeParameter(idx);
                at += 2;
            },
            0x1 => {
                let idx = self.read_u1(at + 1)?;
                typeref = TypeRef::MethodTypeParameter(idx);
                at += 2;
            },
            0x10 => {
                let idx = self.read_u2(at + 1)?;
                typeref = TypeRef::ClassExtends(idx);
                at += 3;
            },
            0x11 => {
                let parameter = self.read_u1(at + 1)?;
                let bound = self.read_u1(at + 2)?;
                typeref = TypeRef::ClassTypeParamenterBound {parameter, bound};
                at += 3;
            },
            0x12 => {
                let parameter = self.read_u1(at + 1)?;
                let bound = self.read_u1(at + 2)?;
                typeref = TypeRef::MethodTypeParameterBound {parameter,  bound};
                at += 3;
            },
            0x13 => {
                typeref = TypeRef::FieldDeclaration;
                at += 1;
            },
            0x14 => {
                typeref = TypeRef::MethodReturnType;
                at += 1;
            },
            0x15 => {
                typeref = TypeRef::MethodRecieverType;
                at += 1;
            },
            0x16 => {
                let idx = self.read_u1(at + 1)?;
                typeref = TypeRef::MethodFormalParameter(idx);
                at += 2;
            },
            0x17 => {
                let idx = self.read_u2(at + 1)?;
                typeref = TypeRef::ThrowsClause(idx);
                at += 3;
            },
            _ => {
                return Err(ClassDecodeError::UnrecognizedTypeRef);
            }
        }
        let tph = self.read_type_path(at)?;
        let type_path = tph.1;
        at = tph.0;
        Ok((at, typeref, type_path))
    }
    fn read_type_path(&self, mut at: usize) -> Result<(usize, TypePath)> {
        let path_len = self.read_u1(at)?;
        at += 1;
        let mut type_path = Vec::new();
        for _ in 0..path_len {
            let discr = self.read_u1(at)?;
            match discr {
                0 => {
                    type_path.push(TypePathEntry::ArrayElement);
                },
                1 => {
                    type_path.push(TypePathEntry::InnerType);
                },
                2 => {
                    type_path.push(TypePathEntry::WildcardBound);
                },
                3 => {
                    let arg = self.read_u1(at + 1)?;
                    type_path.push(TypePathEntry::TypeArgument(arg));
                },
                _ => {
                    return Err(ClassDecodeError::UnrecognizedTypePathKind);
                }
            }
            at += 2;
        }
        Ok((at, type_path))
    }
    fn get_attributes_offset(&self) -> Result<usize> {
        let mut u = self.header_offset + 8 +
            self.read_u2(self.header_offset + 6)? as usize * 2;
        let nfields = self.read_u2(u)?;
        for _ in 0..nfields {
            let n_field_attrs = self.read_u2(u + 8)?;
            for _ in 0..n_field_attrs {
                u += 6 + self.read_u4(u + 12)? as usize;
            }
            u += 8;
        }
        u += 2;
        let nmethods = self.read_u2(u)?;
        for _ in 0..nmethods {
            let n_method_attrs = self.read_u2(u + 8)?;
            for _ in 0..n_method_attrs {
                u += 6 + self.read_u4(u + 12)? as usize;
            }
            u += 8;
        }
        Ok(u + 2)
    }

    fn read_const(&mut self, item: usize) -> Result<ClassConstant> {
        let at = *self.const_pool.get(item).ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let discr = self.read_u1(at)?;
        match discr {
            opcodes::CONSTANT_INTEGER => {
                self.read_i4(at + 1).map(ClassConstant::Integer)
            },
            opcodes::CONSTANT_FLOAT => {
                self.read_f4(at + 1).map(ClassConstant::Float)
            },
            opcodes::CONSTANT_LONG => {
                self.read_i8(at + 1).map(ClassConstant::Long)
            },
            opcodes::CONSTANT_DOUBLE => {
                self.read_f8(at + 1).map(ClassConstant::Double)
            },
            opcodes::CONSTANT_STRING => {
                self.read_utf8(at + 1).map(ClassConstant::String)
            },
            opcodes::CONSTANT_CLASS => {
                self.read_utf8(at + 1)
                    .map(Type::new_object_type)
                    .map(ClassConstant::Class)
            },
            opcodes::CONSTANT_METHODTYPE => {
                self.read_utf8(at + 1)
                    .map(Type::new)
                    .map(ClassConstant::MethodType)
            },
            opcodes::CONSTANT_METHODHANDLE => {
                let kind = self.read_u1(at + 1)?;
                let info = self.read_method_ref(at + 2)?;
                Ok(ClassConstant::MethodHandle(
                    Handle::new(kind, info.0, info.1, info.2)))
            },
            opcodes::CONSTANT_DYNAMIC => {
                Ok(ClassConstant::ConstantDynamic(self.read_indy(at + 1)?))
            },
            _ => {
                Err(ClassDecodeError::UnrecognizedConstantPoolEntry)
            }
        }
    }
}
fn create_label(offset: isize, labels: &mut [Option<Label>]) -> Result<Label> {
    if offset < 0 {
        Err(ClassDecodeError::InvalidCodeOffset)
    } else {
        labels.get_mut(offset as usize)
            .map(|c| *c.get_or_insert(Label::new()))
            .ok_or(ClassDecodeError::InvalidCodeOffset)
    }

}
fn classify_insn(insn: u8) -> Result<InsnClass> {
    INSN_CLASS_TABLE.get(insn as usize).cloned()
        .ok_or(ClassDecodeError::UnrecognizedInstruction)
}
#[derive(Clone, Copy)]
enum InsnClass {
    NoArg,
    Sbyte,
    Short,
    Var,
    ImplVar,
    Type,
    FieldMeth,
    ItfMeth,
    IndyMeth,
    Label,
    WideLabel,
    Ldc,
    LdcWide,
    Iinc,
    Tableswitch,
    Lookupswitch,
    Multianew,
    Wide
}
//I regret nothing
const INSN_CLASS_TABLE: [InsnClass; 202] =
    [InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::Sbyte, InsnClass::Short, InsnClass::Ldc, InsnClass::LdcWide,
     InsnClass::LdcWide, InsnClass::Var, InsnClass::Var, InsnClass::Var,
     InsnClass::Var, InsnClass::Var, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::Var, InsnClass::Var,
     InsnClass::Var, InsnClass::Var, InsnClass::Var, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar,
     InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::ImplVar, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::Iinc, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::Label, InsnClass::Label, InsnClass::Label,
     InsnClass::Label, InsnClass::Label, InsnClass::Label, InsnClass::Label,
     InsnClass::Label, InsnClass::Label, InsnClass::Label, InsnClass::Label,
     InsnClass::Label, InsnClass::Label, InsnClass::Label, InsnClass::Label,
     InsnClass::Label, InsnClass::Var, InsnClass::Tableswitch, InsnClass::Lookupswitch,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg, InsnClass::NoArg,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::FieldMeth, InsnClass::FieldMeth,
     InsnClass::FieldMeth, InsnClass::FieldMeth, InsnClass::FieldMeth,
     InsnClass::FieldMeth, InsnClass::FieldMeth, InsnClass::ItfMeth,
     InsnClass::IndyMeth, InsnClass::Type, InsnClass::Sbyte, InsnClass::Type,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::Type, InsnClass::Type,
     InsnClass::NoArg, InsnClass::NoArg, InsnClass::Wide, InsnClass::Multianew,
     InsnClass::Label, InsnClass::Label, InsnClass::WideLabel, InsnClass::WideLabel];
