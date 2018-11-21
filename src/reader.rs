use super::{ClassVisitor, AnnotationVisitor, MethodVisitor};
use super::{Type, TypeRef,  TypePathEntry, LocalVariableSpan};
use super::{ClassAccess, FieldAccess, MethodAccess, ParameterAccess, InnerClassAccess};
use super::{ModuleFlags, ExportFlags, RequireFlags};
use super::{AnnotationPrimitive, ClassConstant, ConstantDynamic};
use super::{Label, Handle, FrameItem, FrameMode, opcodes};
use std::{char,
    collections::hash_map::{HashMap, Entry},
    rc::Rc
};
use bitflags::*;



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
    InvalidUtf8,
    InconsistentInnerClassAttribute,
    MissingSuperclass
}


pub type Result<T> = ::std::result::Result<T, ClassDecodeError>;

#[derive(Debug)]
pub struct ClassReader<'a> {
    bytes: &'a [u8],
    const_pool: Vec<usize>,
    header_offset: usize,
    error: Option<ClassDecodeError>,
}

mod constant_pool_entry {
    pub(crate) const UTF8:u8 = 1;
    pub(crate) const INT:u8 = 3;
    pub(crate) const FLOAT:u8 = 4;
    pub(crate) const LONG:u8 = 5;
    pub(crate) const DOUBLE:u8 = 6;
    pub(crate) const CLASS:u8 = 7;
    pub(crate) const STR:u8 = 8;
    pub(crate) const FIELD:u8 = 9;
    pub(crate) const METH:u8 = 10;
    pub(crate) const IMETH:u8 = 11;
    pub(crate) const NAME_TYPE:u8 = 12;
    pub(crate) const HANDLE:u8 = 15;
    pub(crate) const MTYPE:u8 = 16;
    pub(crate) const CONDY:u8 = 17;
    pub(crate) const INDY:u8 = 18;
    
    pub(crate) const MODULE:u8 = 19;
    pub(crate) const PACKAGE:u8 = 20;
}

struct DecodeCESU8<I> {
    inner: I,
    err: bool
}
impl<I> DecodeCESU8<I> where I: Iterator<Item = u8> {
    fn new<U>(it: U) -> DecodeCESU8<I> where U:IntoIterator<Item = u8, IntoIter = I> {
        DecodeCESU8 {
            inner: it.into_iter(),
            err: false
        }
    }
}
impl<I> Iterator for DecodeCESU8<I>
    where I: Iterator<Item = u8> {
    type Item = Result<char>;
    fn next(&mut self) -> Option<Result<char>> {
        if self.err {
            return None;
        }
        let a = self.inner.next()? as u32;
        if a == 0 {
            self.err = true;
            return Some(Err(ClassDecodeError::InvalidUtf8))
        }
        if a <= 0x7F {
            return Some(char::from_u32(a).ok_or(ClassDecodeError::InvalidUtf8))
        }
        let b = self.inner.next();
        if b.is_none() {
            self.err = true;
            return Some(Err(ClassDecodeError::InvalidUtf8));
        }
        let b = b.unwrap() as u32;
        if a & 0b11100000 == 0b11000000 {
            if let Some(ch) = char::from_u32(((a & 0x1F) << 6) + (b & 0x3F)) {
                if ch == '\u{0}' || (ch >= '\u{80}' && ch <= '\u{7FF}') {
                    return Some(Ok(ch));
                } else {
                    self.err = true;
                    return Some(Err(ClassDecodeError::InvalidUtf8));
                }
            } else {
                self.err = true;
                return Some(Err(ClassDecodeError::InvalidUtf8));
            }
        }
        let c = self.inner.next();
        if c.is_none() {
            self.err = true;
            return Some(Err(ClassDecodeError::InvalidUtf8));
        }
        let c = c.unwrap() as u32;
        if a & 0b11110000 == 0b11100000 {
            if let Some(ch) = char::from_u32(((a & 0xF) << 12) + ((b & 0x3F) << 6) + (c & 0x3F)) {
                if ch >= '\u{800}' && ch <= '\u{FFFF}' {
                    Some(Ok(ch))
                } else {
                    self.err = true;
                    Some(Err(ClassDecodeError::InvalidUtf8))
                }
            } else {
                self.err = true;
                Some(Err(ClassDecodeError::InvalidUtf8))
            }
        } else {
            let _ = self.inner.next();
            let e = self.inner.next();
            let f = self.inner.next();
            if e.is_none() || f.is_none() {
                self.err = true;
                return Some(Err(ClassDecodeError::InvalidUtf8));
            }
            let e = e.unwrap() as u32;
            let f = f.unwrap() as u32;
            if let Some(ch) = char::from_u32(0x10000 + ((b & 0x0f) << 16) + ((c & 0x3f) << 10) + ((e & 0x0f) << 6) + (f & 0x3f)) {
                if ch > '\u{FFFF}' {
                    Some(Ok(ch))
                } else {
                    self.err = true;
                    Some(Err(ClassDecodeError::InvalidUtf8))
                }
            } else {
                self.err = true;
                Some(Err(ClassDecodeError::InvalidUtf8))
            }
        }
    }
}

fn into_handle(cs: ClassConstant) -> Handle {
    if let ClassConstant::MethodHandle(hn) = cs {
        hn
    } else {
        panic!("");
    }
}

impl<'a> ClassReader<'a> {
    pub fn new<'b>(bytes: &'b [u8]) -> ClassReader<'b> {
        let ret = ClassReader::new_innner(bytes);
        if let Ok(this) = ret {
            this
        } else {
            ClassReader {
                error: Some(ret.unwrap_err()),
                bytes,
                const_pool: Vec::new(),
                header_offset: 0
            }
        }
    }
    fn new_innner<'b>(bytes: &'b [u8]) -> Result<ClassReader<'b>> {
        let mut this = ClassReader {
            bytes,
            const_pool: vec![],
            header_offset: 0,
            error: None
        };
        let magic = this.read_u4(0)?;
        if magic != opcodes::CLASS_MAGIC {
            return Err(ClassDecodeError::InvalidMagic);
        }
        let version = this.read_u2(6)?;
        if version > opcodes::V11 {
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
                constant_pool_entry::FIELD |
                constant_pool_entry::METH |
                constant_pool_entry::IMETH |
                constant_pool_entry::INT |
                constant_pool_entry::FLOAT |
                constant_pool_entry::NAME_TYPE |
                constant_pool_entry::INDY |
                constant_pool_entry::CONDY => {
                    offset += 5;
                },
                constant_pool_entry::LONG |
                constant_pool_entry::DOUBLE => {
                    offset += 9;
                    this.const_pool.push(0);
                    i += 1;
                },
                constant_pool_entry::HANDLE => {
                    offset += 4;
                },
                constant_pool_entry::CLASS |
                constant_pool_entry::STR |
                constant_pool_entry::MTYPE |
                constant_pool_entry::PACKAGE |
                constant_pool_entry::MODULE => {
                    offset += 3;
                },
                constant_pool_entry::UTF8 => {
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
        self.bytes.get(idx).map(|c| *c).ok_or(ClassDecodeError::UnexpectedEOF)
    }
    fn read_u2(&self, idx: usize) -> Result<u16> {
        let i = self.bytes.get(idx..idx + 2).ok_or(ClassDecodeError::UnexpectedEOF)?;
        let merged = ((i[0] as u16) << 8) | (i[1] as u16);
        Ok(merged)
    }
    fn read_u4(&self, idx: usize) -> Result<u32> {
        let i = self.bytes.get(idx..idx + 4).ok_or(ClassDecodeError::UnexpectedEOF)?;
        let merged = ((i[0] as u32) << 24) | ((i[1] as u32) << 16) | ((i[2] as u32) << 8) | (i[3] as u32);
        Ok(merged)
    }
    fn read_u8(&self, idx: usize) -> Result<u64> {
        let l1 = self.read_u4(idx)? as u64;
        let l0 = self.read_u4(idx + 4)? as u64;
        Ok((l1 << 32) | l0)
    }
    fn read_f8(&self, idx: usize) -> Result<f64> {
        self.read_u8(idx).map(|c| f64::from_bits(c))
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
        self.read_u4(idx).map(|c| f32::from_bits(c))
    }
    fn read_stringlike(&self, idx: usize, kind: u8, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Option<Rc<str>>> {
        let pool_index = self.read_u2(idx)? as usize;
        if pool_index == 0 {
            return Ok(None);
        }
        let mut item_offset = self.const_pool.get(pool_index).map(|c| *c).ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        if self.read_u1(item_offset)? != kind {
            return Err(ClassDecodeError::ConstantPoolTypeMismatch);
        }
        item_offset += 1;
        self.read_utf8(item_offset, string_cache).map(|e| Some(e))
    }
    fn read_class_maybe(&self, idx: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Option<Rc<str>>> {
        self.read_stringlike(idx, constant_pool_entry::CLASS, string_cache)
    }
    fn read_module(&self, idx: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Rc<str>> {
        let md = self.read_stringlike(idx, constant_pool_entry::MODULE, string_cache)?;
        md.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_class(&self, idx:usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Rc<str>> {
        let cl = self.read_class_maybe(idx, string_cache)?;
        cl.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_package(&self, idx:usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Rc<str>> {
        let pk = self.read_stringlike(idx, constant_pool_entry::PACKAGE, string_cache)?;
        pk.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_utf8(&self, idx: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Rc<str>> {
        let utf = self.read_utf8_maybe(idx, string_cache)?;
        utf.ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)
    }
    fn read_utf8_maybe(&'a self, idx: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Option<Rc<str>>> {
        let pool_index = self.read_u2(idx)? as usize;
        if pool_index == 0 {
            return Ok(None);
        }
        let offset = self.const_pool.get(pool_index).map(|c| *c).ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let ent = string_cache.entry(offset);
        let sth = if let Entry::Vacant(_) = ent {
            if self.read_u1(offset)? != constant_pool_entry::UTF8 {
                return Err(ClassDecodeError::ConstantPoolTypeMismatch)
            }
            let len = self.read_u2(offset + 1)? as usize;
            let strn = self.read_cesu8(offset + 3, len)?;
            ent.or_insert(Rc::from(strn))
        } else {
            ent.or_insert_with(|| unreachable!())
        };
        Ok(Some(sth.clone()))
    }
    fn read_cesu8(&self, data_offset: usize, data_size: usize) -> Result<String> {
        let slc = &self.bytes[data_offset..data_offset+data_size];
        DecodeCESU8::new(slc.iter().map(|c| *c)).collect::<Result<String>>()
    }
    fn read_name_and_type_maybe(&self, at: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<Option<(Rc<str>, Rc<str>)>> {
        let item = self.read_u2(at)? as usize;
        if item != 0 {
            let &cpi = self.const_pool.get(item)
                .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
            if self.read_u1(cpi)? != constant_pool_entry::NAME_TYPE {
                return Err(ClassDecodeError::ConstantPoolTypeMismatch)
            }
            let enclosing_name = self.read_utf8(cpi + 1, string_cache)?;
            let enclosing_desc = self.read_utf8(cpi + 3, string_cache)?;
            Ok(Some((enclosing_name, enclosing_desc)))
        } else {
            Ok(None)
        }
    }
    fn read_name_and_type(&self, at: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<(Rc<str>, Rc<str>)> {
        let rs = self.read_name_and_type_maybe(at, string_cache)?;
        if let Some(r) = rs {
            Ok(r)
        } else {
            Err(ClassDecodeError::ConstantPoolIndexOutOfBounds)
        }
    }
    fn read_method_ref(&self, at: usize, string_cache: &mut HashMap<usize, Rc<str>>) -> Result<(Rc<str>, (Rc<str>, Rc<str>))> {
        let item = self.read_u2(at)? as usize;
        let cpi = self.const_pool.get(item)
            .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let owner = self.read_class(cpi + 1, string_cache)?;
        let nat = self.read_name_and_type(cpi + 3, string_cache)?;
        Ok((owner, nat))
    }
    pub fn get_name(&self) -> Result<Rc<str>> {
        let mut string_cache = HashMap::new();
        if self.error.is_some() {
            return Err(*self.error.as_ref().unwrap());
        }
        let hdr = self.header_offset;
        Ok(self.read_class(hdr + 2, &mut string_cache)?)
    }
    pub fn get_access(&self) -> Result<ClassAccess> {
        let access_raw = self.read_u2(self.header_offset)?;
        Ok(ClassAccess::from_bits_truncate(access_raw as u32))
    }
    pub fn accept(&'a self, visitor: &'a mut dyn ClassVisitor, flags: ClassReaderFlags) -> Result<()> {
        let mut string_cache = HashMap::new();
        if self.error.is_some() {
            return Err(*self.error.as_ref().unwrap());
        }
        let mut hdr = self.header_offset;
        let version_minor = self.read_u2(4)?;
        let version_major = self.read_u2(6)?;
        let mut access = self.get_access()?;
        let name = self.read_class(hdr + 2, &mut string_cache)?;
        let super_class = self.read_class_maybe(hdr + 4, &mut string_cache)?;
        if super_class.is_none() && &*name != "java/lang/Object" {
            return Err(ClassDecodeError::MissingSuperclass);
        }
        let num_interfaces = self.read_u2(hdr + 6)?;
        let mut interfaces = Vec::new();
        hdr += 8;
        for _ in 0..num_interfaces {
            interfaces.push(self.read_class(hdr, &mut string_cache)?);
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
        let mut bootstrap_methods = Vec::new();
        let mut nest_host = None;
        let mut nest_members_offset = 0;
        let num_attrs = self.read_u2(hdr)?;
        for _ in 0..num_attrs {
            let attr_name = &*self.read_utf8(hdr + 2, &mut string_cache)?;
            match attr_name {
                "SourceFile" => {
                    source_file = Some(self.read_utf8(hdr + 8, &mut string_cache)?)
                },
                "InnerClasses" => {
                    inner_classes = hdr + 8;
                },
                "EnclosingMethod" => {
                    enclosing_class = Some(self.read_class(hdr + 8, &mut string_cache)?);
                    enclosing_method = self.read_name_and_type_maybe(hdr + 10, &mut string_cache)?;
                },
                "Signature" => {
                    signature = Some(self.read_utf8(hdr + 8, &mut string_cache)?);
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
                    bootstrap_methods.reserve(num_meth as usize);
                    let mut nat = hdr + 10;
                    for _ in 0..num_meth {
                        bootstrap_methods.push(nat);
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
                    nest_host = Some(self.read_class(hdr + 8, &mut string_cache)?);
                },
                "NestMembers" => {
                    nest_members_offset = hdr + 8;
                },
                "Module" => {
                    module = hdr + 8;
                },
                "ModuleMainClass" => {
                    module_main = Some(self.read_class(hdr + 8, &mut string_cache)?);
                },
                "ModulePackages" => {
                    module_packages = hdr + 10;
                },
                _ => {

                }
            }
            hdr += 6 + self.read_u4(hdr + 4)? as usize;
        }
        visitor.visit_header(version_minor, version_major, access, name, signature, super_class, interfaces);
        if !flags.contains(ClassReaderFlags::SKIP_DEBUG) {
            visitor.visit_source(source_file, source_debug);
        }
        if module != 0 {
            let mut at = module;
            let name = self.read_module(at, &mut string_cache)?;
            let flags = ModuleFlags::from_bits_truncate(self.read_u2(at + 2)? as u32);
            let version = self.read_utf8_maybe(at + 4, &mut string_cache)?;
            let mv = visitor.visit_module(name, flags, version);
            at += 6;
            if let Some(mv) = mv {
                if module_main.is_some() {
                    mv.visit_main_class(module_main.unwrap());
                }
                if module_packages != 0 {
                    let num = self.read_u2(module_packages - 2)? as usize;
                    for i in 0..num {
                        let pkg = self.read_package(module_packages + i * 2, &mut string_cache)?;
                        mv.visit_package(pkg);
                    }
                }
                let num_req = self.read_u2(at)?;
                at += 2;
                for _ in 0..num_req {
                    let md = self.read_module(at, &mut string_cache)?;
                    let flags = RequireFlags::from_bits_truncate(self.read_u2(at + 2)? as u32);
                    let version = self.read_utf8_maybe(at + 4, &mut string_cache)?;
                    mv.visit_require(md, flags, version);
                    at += 6;
                }
                let num_exp = self.read_u2(at)?;
                at += 2;
                for _ in 0..num_exp {
                    let export = self.read_package(at, &mut string_cache)?;
                    let access = ExportFlags::from_bits_truncate(self.read_u2(at + 2)? as u32);
                    let num_export_to = self.read_u2(at + 4)? as usize;
                    at += 6;
                    let mut export_to = Vec::with_capacity(num_export_to);
                    for _ in 0..num_export_to {
                        let s = self.read_module(at, &mut string_cache)?;
                        export_to.push(s);
                        at += 2;
                    }
                    mv.visit_export(export, access, export_to);
                }
                let num_open = self.read_u2(at)?;
                at += 2;
                for _ in 0..num_open {
                    let open = self.read_package(at, &mut string_cache)?;
                    let access = ExportFlags::from_bits_truncate(self.read_u2(at + 2)? as u32);
                    let num_open_to = self.read_u2(at + 4)? as usize;
                    at += 6;
                    let mut open_to = Vec::with_capacity(num_open_to);
                    for _ in 0..num_open_to {
                        let s = self.read_module(at, &mut string_cache)?;
                        open_to.push(s);
                        at += 2;
                    }
                    mv.visit_open(open, access, open_to);
                }
                let num_use = self.read_u2(at)?;
                at += 2;
                for _ in 0..num_use {
                    let cl = self.read_class(at, &mut string_cache)?;
                    at += 2;
                    mv.visit_use(cl);
                }
                let num_provide = self.read_u2(at)?;
                at += 2;
                for _ in 0..num_provide {
                    let service = self.read_class(at, &mut string_cache)?;
                    let num_provide_with = self.read_u2(at + 2)? as usize;
                    at += 4;
                    let mut provide_with = Vec::with_capacity(num_provide_with);
                    for _ in 0..num_provide_with {
                        let s = self.read_class(at, &mut string_cache)?;
                        provide_with.push(s);
                        at += 2;
                    }
                    mv.visit_provide(service, provide_with);
                }
                mv.visit_end();
            }
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
                let desc = self.read_utf8(v, &mut string_cache)?;
                let mut vis = visitor.visit_annotation(desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis, &mut string_cache, &bootstrap_methods)?;
            }
        }
        if invisible_annotations_offset != 0 {
            let num = self.read_u2(invisible_annotations_offset)?;
            let mut v = invisible_annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v, &mut string_cache)?;
                let mut vis = visitor.visit_annotation(desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis, &mut string_cache, &bootstrap_methods)?;
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
                let desc = self.read_utf8(v, &mut string_cache)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis, &mut string_cache, &bootstrap_methods)?
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
                let desc = self.read_utf8(v, &mut string_cache)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis, &mut string_cache, &bootstrap_methods)?
            }
        }
        if nest_members_offset != 0 {
            let num = self.read_u2(nest_members_offset)?;
            let mut offset = nest_members_offset + 2;
            for _ in 0..num {
                let class = self.read_class(offset, &mut string_cache)?;
                visitor.visit_nest_member(class);
                offset += 2;
            }
        }
        if inner_classes != 0 {
            let mut offset = inner_classes + 2;
            let num = self.read_u2(inner_classes)?;
            for _ in 0..num {
                let inner = self.read_class(offset, &mut string_cache)?;
                let outer = self.read_class_maybe(offset + 2, &mut string_cache)?;
                let inner_name = self.read_utf8_maybe(offset + 4, &mut string_cache)?;
                if version_major >= opcodes::V1_7 {
                    if inner_name.is_none() && outer.is_some() {
                        return Err(ClassDecodeError::InconsistentInnerClassAttribute);
                    }
                }
                let access = InnerClassAccess::from_bits_truncate(
                    self.read_u2(offset + 6)? as u32);
                visitor.visit_inner_class(inner, outer, inner_name, access);
                offset += 8;
            }
        }
        let mut at = self.header_offset + 10 + 2 * (num_interfaces as usize);
        let nfields = self.read_u2(at - 2)?;
        for _ in 0..nfields {
            at = self.read_field(visitor, at, &mut string_cache, &bootstrap_methods)?;
        }
        let nmethods = self.read_u2(at)?;
        at += 2;
        for _ in 0..nmethods {
            at = self.read_method(visitor, at, flags, &bootstrap_methods, &mut string_cache)?;
        }
        visitor.visit_end();
        Ok(())
    }

    fn read_field(&self, vis: &mut dyn ClassVisitor, mut at: usize, string_cache: &mut HashMap<usize, Rc<str>>,
                  bootstrap_methods: &[usize]) -> Result<usize> {
        let access_raw = self.read_u2(at)? as u32;
        let mut access = FieldAccess::from_bits_truncate(access_raw);
        let name = self.read_utf8(at + 2, string_cache)?;
        let desc = self.read_utf8(at + 4, string_cache)?;
        at += 6;
        let num_attrs = self.read_u2(at)?;
        let mut signature = None;
        let mut annotations_offset = 0;
        let mut invisible_annotations_offset = 0;
        let mut type_annotations_offset = 0;
        let mut invisible_type_annotations_offset = 0;
        let mut value = None;
        for _ in 0..num_attrs {
            let attr_name = &*self.read_utf8(at + 2, string_cache)?;
            match attr_name {
                "ConstantValue" => {
                    let itm = self.read_u2(at + 8)? as usize;
                    if itm != 0 {
                        value = Some(self.read_const(itm, string_cache, bootstrap_methods)?);
                    }
                },
                "Signature" => {
                    signature = Some(self.read_utf8(at + 8, string_cache)?);
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
        let fv = vis.visit_field(access, name, desc, signature, value);
        if let Some(fv) = fv {
            if annotations_offset != 0 {
                let num = self.read_u2(annotations_offset)?;
                let mut v = annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v, string_cache)?;
                    let mut vis = fv.visit_annotation(desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?;
                }
            }
            if invisible_annotations_offset != 0 {
                let num = self.read_u2(invisible_annotations_offset)?;
                let mut v = invisible_annotations_offset + 2;
                for _ in 0..num {
                    let desc = self.read_utf8(v, string_cache)?;
                    let mut vis = fv.visit_annotation(desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?;
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
                    let desc = self.read_utf8(v, string_cache)?;
                    let mut vis = fv.visit_type_annotation(type_ref, type_path, desc, true);
                    v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?
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
                    let desc = self.read_utf8(v, string_cache)?;
                    let mut vis = fv.visit_type_annotation(type_ref, type_path, desc, false);
                    v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?
                }
            }
            fv.visit_end();
        }
        Ok(at)
    }
    fn read_method(&self, vis: &mut dyn ClassVisitor, mut at: usize, flags: ClassReaderFlags, bootstrap_methods: &[usize],
                   string_cache: &mut HashMap<usize, Rc<str>>) -> Result<usize> {
        let access_raw = self.read_u2(at)? as u32;
        let mut access = MethodAccess::from_bits_truncate(access_raw);
        let name = self.read_utf8(at + 2, string_cache)?;
        let desc = self.read_utf8(at + 4, string_cache)?;
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
            let attr_name = self.read_utf8(at + 2, string_cache)?;
            match &*attr_name {
                "Code" => {
                    if !flags.contains(ClassReaderFlags::SKIP_CODE) {
                        code = at + 8;
                    }
                },
                "Exceptions" => {
                    let n_exceptions = self.read_u2(at + 8)?;
                    let mut exc_ref = at + 10;
                    for _ in 0..n_exceptions {
                        let name = self.read_class(exc_ref, string_cache)?;
                        exceptions.push(name);
                        exc_ref += 2;
                    }
                },
                "Signature" => {
                    signature = Some(self.read_utf8(at + 8, string_cache)?);
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
        let visitor = if let Some(q) = vis.visit_method(access, name, desc, signature, exceptions) {
            q
        } else {
            return Ok(at);
        };
        if method_parameters != 0 {
            let count = self.read_u1(method_parameters)? as usize;
            for i in 0..count {
                let name = self.read_utf8(method_parameters + 1 + i * 4, string_cache)?;
                let access_raw = self.read_u2(method_parameters + 3 + i * 4)? as u32;
                let access = ParameterAccess::from_bits_truncate(access_raw);
                visitor.visit_parameter(name, access);
            }
        }
        if annotation_default != 0 {
            let mut vis = visitor.visit_annotation_default();
            self.read_annotation_value(annotation_default, Rc::from(""), &mut vis, string_cache, bootstrap_methods)?;
            vis.map(|c| c.visit_end());
        }
        if annotations_offset != 0 {
            let num = self.read_u2(annotations_offset)?;
            let mut v = annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v, string_cache)?;
                let mut vis = visitor.visit_annotation(desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?;
            }
        }
        if invisible_annotations_offset != 0 {
            let num = self.read_u2(invisible_annotations_offset)?;
            let mut v = invisible_annotations_offset + 2;
            for _ in 0..num {
                let desc = self.read_utf8(v, string_cache)?;
                let mut vis = visitor.visit_annotation(desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?;
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
                let desc = self.read_utf8(v, string_cache)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, true);
                v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?
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
                let desc = self.read_utf8(v, string_cache)?;
                let mut vis = visitor.visit_type_annotation(type_ref, type_path, desc, false);
                v = self.read_annotation_values(v + 2, true, &mut vis, string_cache, bootstrap_methods)?
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
                    let desc = self.read_utf8(at, string_cache)?;
                    let mut vis = visitor.visit_parameter_annotation(i, desc, true);
                    at = self.read_annotation_values(at + 2, true, &mut vis, string_cache, bootstrap_methods)?
                }
            }
        }
        if invisible_parameter_annotations_offset != 0 {
            let mut at = invisible_parameter_annotations_offset;
            let pars = self.read_u1(at)?;
            visitor.visit_annotable_parameter_count(pars, true);
            at += 1;
            for i in 0..pars {
                let num = self.read_u2(at)?;
                at += 2;
                for _ in 0..num {
                    let desc = self.read_utf8(at, string_cache)?;
                    let mut vis = visitor.visit_parameter_annotation(i, desc, false);
                    at = self.read_annotation_values(at + 2, true, &mut vis, string_cache, bootstrap_methods)?
                }
            }
        }
        if code == 0 {
            visitor.visit_end();
            return Ok(at);
        }
        visitor.visit_code();
        self.read_code(visitor, code, flags, bootstrap_methods, string_cache)?;
        visitor.visit_end();
        Ok(at)
    }

    fn read_code(&self, vis: &mut dyn MethodVisitor, mut at: usize, flags: ClassReaderFlags, bootstrap_methods: &[usize],
                 string_cache: &mut HashMap<usize, Rc<str>>) -> Result<()> {
        let max_stack = self.read_u2(at)?;
        let max_local = self.read_u2(at + 2)?;
        let code_length = self.read_u4(at + 4)? as usize;
        if code_length >= 65536 {
            return Err(ClassDecodeError::BytecodeTooLong);
        }
        at += 8;
        let code_begin = at;
        let code_end = at + code_length;
        let mut labels = vec![None; code_length + 2];
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
        let num_try_catch = self.read_u2(at)?;
        for _ in 0..num_try_catch {
            let start = create_label(self.read_u2(at + 2)? as isize, &mut labels)?;
            let end = create_label(self.read_u2(at + 4)? as isize, &mut labels)?;
            let handler = create_label(self.read_u2(at + 6)? as isize, &mut labels)?;
            let class = self.read_class_maybe(at + 8, string_cache)?;
            vis.visit_try_catch(start, end, handler, class);
            at += 8;
        }
        at += 2;
        let nattr = self.read_u2(at)?;
        let mut variables_table = 0;
        let mut varitables_type_table = 0;
        let mut line_numbers = HashMap::new();
        let mut stack_map = 0;
        let mut stack_map_size = 0;
        let mut frame_count = 0;
        let mut local_var_annot = Vec::new();
        let mut insn_annot = HashMap::new();
        for _ in 0..nattr {
            let name = self.read_utf8(at + 2, string_cache)?;
            match &*name {
                "LocalVariableTable" => {
                    if !flags.contains(ClassReaderFlags::SKIP_DEBUG) {
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
                    }
                },
                "LocalVariableTypeTable" => {
                    varitables_type_table = at + 8;
                },
                "LineNumberTable" => {
                    if !flags.contains(ClassReaderFlags::SKIP_DEBUG) {
                        let table_len = self.read_u2(at + 8)?;
                        let mut nat = at;
                        for _ in 0..table_len {
                            let addr = self.read_u2(nat + 10)?;
                            create_label(addr as isize, &mut labels)?;
                            let lb = labels[addr as usize].unwrap();
                            let line = self.read_u2(nat + 12)?;
                            line_numbers.entry(lb).or_insert(Vec::new()).push(line);
                            nat += 4;
                        }
                    }
                },
                "RuntimeVisibleTypeAnnotations" | "RuntimeInvisibleTypeAnnotations" => {
                    let mut nat = at + 8;
                    let n_annot = self.read_u2(nat)?;
                    nat += 2;
                    let visible = &*name == "RuntimeVisibleTypeAnnotations";
                    for _ in 0..n_annot {
                        let target = self.read_u1(nat)?;
                        nat += 1;
                        match target {
                            0x40 | 0x41 => {
                                let table_len = self.read_u2(nat)? as usize;
                                let mut ranges = Vec::with_capacity(table_len);
                                nat += 2;
                                for _ in 0..table_len {
                                    let start_offset = self.read_u2(nat)? as isize;
                                    let start = create_label(start_offset, &mut labels)?;
                                    let end = create_label(start_offset + self.read_u2(nat + 2)? as isize, &mut labels)?;
                                    let var = self.read_u2(nat + 4)?;
                                    nat += 6;
                                    ranges.push(LocalVariableSpan::new(start, end, var));
                                }
                                let tph = self.read_type_path(nat)?;
                                nat = tph.0;
                                let type_path = tph.1;
                                let kind = if target == 0x40 { TypeRef::LocalVariable } else { TypeRef::ResourceVariable };
                                local_var_annot.push((kind, type_path, ranges, nat, visible));
                                nat = self.read_annotation_values(nat + 2, true, &mut None, string_cache, bootstrap_methods)?;
                            },
                            0x42 => {
                                let catch_index = TypeRef::ThrowsClause(self.read_u2(nat)?);
                                nat += 2;
                                let tph = self.read_type_path(nat)?;
                                nat = tph.0;
                                let type_path = tph.1;
                                let desc = self.read_utf8(nat, string_cache)?;
                                nat += 2;
                                let mut av = vis.visit_trycatch_annotation(catch_index, type_path, desc, visible);
                                nat = self.read_annotation_values(nat, true, &mut av, string_cache, bootstrap_methods)?;
                            },
                            0x43 | 0x44 | 0x45 | 0x46 => {
                                let insn_offset = self.read_u2(nat)? as usize;
                                nat += 2;
                                let tph = self.read_type_path(nat)?;
                                nat = tph.0;
                                let type_path = tph.1;
                                let kind = match target {
                                    0x43 => TypeRef::Instanceof,
                                    0x44 => TypeRef::New,
                                    0x45 => TypeRef::ContructorReference,
                                    0x46 => TypeRef::MethodReference,
                                    _ => unreachable!()
                                };
                                insn_annot.entry(insn_offset).or_insert(Vec::new()).push((kind, type_path, nat, visible));
                                nat = self.read_annotation_values(nat + 2, true, &mut None, string_cache, bootstrap_methods)?;
                            },
                            0x47 | 0x48 | 0x49 | 0x4A | 0x4B => {
                                let insn_offset = self.read_u2(nat)? as usize;
                                nat += 2;
                                let type_arg = self.read_u1(nat)?;
                                nat += 1;
                                let tph = self.read_type_path(nat)?;
                                nat = tph.0;
                                let type_path = tph.1;
                                let kind = match target {
                                    0x47 => TypeRef::Cast(type_arg),
                                    0x48 => TypeRef::GenericConstructorInvocation(type_arg),
                                    0x49 => TypeRef::GenericMethodInvocation(type_arg),
                                    0x4A => TypeRef::GenericConstructorReference(type_arg),
                                    0x4B => TypeRef::GenericMethodReference(type_arg),
                                    _ => unreachable!()
                                };
                                insn_annot.entry(insn_offset).or_insert(Vec::new()).push((kind, type_path, nat, visible));
                                nat = self.read_annotation_values(nat + 2, true, &mut None, string_cache, bootstrap_methods)?;
                            },
                            _ => {
                                return Err(ClassDecodeError::UnrecognizedTypeRef);
                            }
                        }

                    }
                },
                "StackMapTable" => {
                    if !flags.contains(ClassReaderFlags::SKIP_FRAMES) {
                        stack_map = at + 10;
                        stack_map_size = self.read_u4(at + 4)? as usize;
                        frame_count = self.read_u2(at + 8)?;
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
        if stack_map != 0 {
            for i in stack_map..(stack_map + stack_map_size - 2) {
                if self.read_u1(i)? == 8 {
                    let v = self.read_u2(i + 1)? as usize;
                    if v < code_length {
                        if self.read_u1(code_begin + v)? == opcodes::NEW {
                            create_label(v as isize, &mut labels)?;
                        }
                    }
                }
            }
        }
        at = code_begin;
        while at < code_end {
            let offset = at - code_begin;
            let lb = labels[offset];
            if let Some(l) = lb {
                vis.visit_label(l);
                if let Some(vc) = line_numbers.get(&l) {
                    for &line in vc {
                        vis.visit_line_number(line, l);
                    }
                }
            }

            while !ran_out_of_frames &&
                (frame_offset == offset || frame_offset == usize::max_value()) {
                    if frame_offset != usize::max_value() {

                        vis.visit_frame(frame_mode, local_count, &locals[0..local_count as usize], stack_count, &stack[0..stack_count as usize]);
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
                            stack_map = self.read_frame_type(&mut stack, 0, stack_map, &labels, string_cache)?;
                        } else {
                            delta = self.read_u2(stack_map)? as usize;
                            stack_map += 2;
                            if tag == SAME_LOCALS_1_STACK_ITEM_EXTENDED {
                                stack_map = self.read_frame_type(&mut stack, 0, stack_map, &labels, string_cache)?;
                                stack_count = 1;
                                frame_mode = FrameMode::Same1;
                            } else if tag >= CHOP_FRAME && tag < SAME_FRAME_EXTENDED {
                                frame_mode = FrameMode::Chop;
                                local_count -= (SAME_FRAME_EXTENDED - tag) as u16;
                                stack_count = 0;
                            } else if tag == SAME_FRAME_EXTENDED {
                                stack_count = 0;
                                frame_mode = FrameMode::Same;
                            } else if tag < FULL_FRAME {
                                for i in 0..(tag - SAME_FRAME_EXTENDED) {
                                    stack_map = self.read_frame_type(&mut locals, i as usize, stack_map, &labels, string_cache)?;
                                }
                                frame_mode = FrameMode::Append;
                                local_count += (tag - SAME_FRAME_EXTENDED) as u16;
                                stack_count = 0;
                            } else {
                                frame_mode = FrameMode::Full;
                                local_count = self.read_u2(stack_map)?;
                                stack_map += 2;
                                for i in 0..local_count {
                                    stack_map = self.read_frame_type(&mut locals, i as usize, stack_map, &labels, string_cache)?;
                                }
                                stack_count = self.read_u2(stack_map)?;
                                stack_map += 2;
                                for i in 0..stack_count {
                                    stack_map = self.read_frame_type(&mut stack, i as usize, stack_map, &labels, string_cache)?;
                                }
                            }

                        }
                        frame_offset = frame_offset.wrapping_add(delta + 1);
                        frame_count -= 1;
                    } else {
                        ran_out_of_frames = true;
                    }
                }

            let opcode = self.read_u1(at)?;
            match classify_insn(opcode)? {
                InsnClass::NoArg => {
                    vis.visit_insn(opcode);
                    at += 1;
                },
                InsnClass::ImplVar => {
                    if opcode > opcodes::ISTORE {
                        let opcode_real = opcodes::ISTORE + ((opcode - 59) >> 2);
                        // ^ISTORE_0
                        vis.visit_var_insn(opcode_real, (opcode - 59 & 0x3) as u16);
                    } else {
                        let opcode_real = opcodes::ILOAD + ((opcode - 26) >> 2);
                        // ^ILOAD_0
                        vis.visit_var_insn(opcode_real, (opcode - 26 & 0x3) as u16);
                    }
                    at += 1;
                },
                InsnClass::Label => {
                    let os = offset as isize + self.read_i2(at + 1)? as isize;
                    let target = labels[os as usize];
                    vis.visit_jump_insn(opcode, target.unwrap());
                    at += 3;
                },
                InsnClass::WideLabel => {
                    let os = offset as isize + self.read_i4(at + 1)? as isize;
                    let target = labels[os as usize];
                    vis.visit_jump_insn(opcode, target.unwrap());
                    at += 5;
                },
                InsnClass::Wide => {
                    let opcode = self.read_u1(at + 1)?;
                    if opcode == opcodes::IINC {
                        let var = self.read_u2(at + 2)?;
                        let by = self.read_i2(at + 4)?;
                        vis.visit_iinc_insn(var, by);
                        at += 6;
                    } else {
                        vis.visit_var_insn(opcode, self.read_u2(at + 2)?);
                        at += 4;
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
                },
                InsnClass::Var => {
                    vis.visit_var_insn(opcode, self.read_u1(at + 1)? as u16);
                    at += 2;
                },
                InsnClass::Sbyte => {
                    let operand = self.read_i1(at + 1)? as i32;
                    vis.visit_int_insn(opcode, operand);
                    at += 2;
                },
                InsnClass::Ldc => {
                    let idx = self.read_u1(at + 1)?;
                    let cst = self.read_const(idx as usize, string_cache, bootstrap_methods)?;
                    vis.visit_ldc_insn(cst);
                    at += 2;
                },
                InsnClass::Short => {
                    let operand = self.read_i2(at + 1)? as i32;
                    vis.visit_int_insn(opcode, operand);
                    at += 3;
                },
                InsnClass::Iinc => {
                    let var = self.read_u1(at + 1)? as u16;
                    let by = self.read_i1(at + 2)? as i16;
                    vis.visit_iinc_insn(var, by);
                    at += 3;
                },
                InsnClass::LdcWide => {
                    let idx = self.read_u2(at + 1)?;
                    let cst = self.read_const(idx as usize, string_cache, bootstrap_methods)?;
                    vis.visit_ldc_insn(cst);
                    at += 3;
                },
                InsnClass::ItfMeth |
                InsnClass::FieldMeth => {
                    let mdata = self.read_method_ref(at + 1, string_cache)?;
                    vis.visit_field_method_insn(opcode, mdata.0,
                                                (mdata.1).0, (mdata.1).1);
                    if opcode == opcodes::INVOKEINTERFACE {
                        at += 5;
                    } else {
                        at += 3;
                    }
                },
                InsnClass::IndyMeth => {
                    let dta = self.read_indy(at + 1, bootstrap_methods, string_cache)?;
                    vis.visit_indy_insn(dta.name, dta.desc, dta.bootstrap, dta.args);
                    at += 5;
                },
                InsnClass::Type => {
                    let cls = self.read_class(at + 1, string_cache)?;
                    vis.visit_type_insn(opcode, cls);
                    at += 3;
                },
                InsnClass::Multianew => {
                    let class = self.read_class(at + 1, string_cache)?;
                    let dim = self.read_u1(at + 3)?;
                    vis.visit_multianew_insn(class, dim);
                    at += 4;
                }
            }
            if let Some(annots) = insn_annot.remove(&offset) {
                for (kind, type_path, mut nat, visible) in annots.into_iter() {
                    let desc = self.read_utf8(nat, string_cache)?;
                    nat += 2;
                    let mut av = vis.visit_insn_annotation(kind, type_path, desc, visible);
                    self.read_annotation_values(nat, true, &mut av, string_cache, bootstrap_methods)?;
                }
            }
        }
        if labels[code_length].is_some() {
            vis.visit_label(labels[code_length].unwrap());
        }
        if variables_table != 0 {
            let mut type_table = None;
            let mut tt_len = 0;
            if varitables_type_table != 0 {
                at = varitables_type_table + 2;
                tt_len = (self.read_u2(varitables_type_table)? * 3) as usize;
                let mut tt = Vec::with_capacity(tt_len as usize);
                for _ in 0..(tt_len / 3) {
                    tt.push(at + 6);
                    tt.push(self.read_u2(at + 8)? as usize);
                    tt.push(self.read_u2(at)? as usize);
                    at += 10;
                }
                type_table = Some(tt);
            }
            at = variables_table + 2;
            for _ in 0..self.read_u2(variables_table)? {
                let start = self.read_u2(at)? as usize;
                let len = self.read_u2(at + 2)? as usize;
                let idx = self.read_u2(at + 8)?;
                let mut sig = None;
                if let Some(ref tt) = type_table {
                    for i in (0..tt_len).step_by(3) {
                        if tt[i] == start && tt[i + 1] == idx as usize {
                            sig = Some(self.read_utf8(tt[i + 2], string_cache)?);
                        }
                    }
                }
                let name = self.read_utf8(at + 4, string_cache)?;
                let desc = self.read_utf8(at + 6, string_cache)?;
                let span = LocalVariableSpan::new(labels[start].unwrap(), labels[start + len].unwrap(), idx);
                vis.visit_local_var(name, desc, sig, span);
                at += 10;
            }
        }

        for (type_ref, type_path, ranges, nat, visible) in local_var_annot {
            let name = self.read_utf8(nat, string_cache)?;
            let mut av = vis.visit_local_variable_annotation(type_ref, type_path, ranges, name, visible);
            self.read_annotation_values(nat + 2, true, &mut av, string_cache, bootstrap_methods)?;
        }
        vis.visit_maxs(max_stack, max_local);
        Ok(())
    }
    fn read_frame_type(&'a self, frame: &mut [FrameItem], idx: usize, mut at: usize, labels: &[Option<Label>],
                       string_cache: &mut HashMap<usize, Rc<str>>) -> Result<usize> {
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
                let cs = self.read_class(at, string_cache)?;
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
    fn read_indy(&self, at: usize, bootstrap_methods: &[usize], string_cache: &mut HashMap<usize, Rc<str>>)
                 -> Result<ConstantDynamic> {
        let cpi = self.const_pool.get(self.read_u2(at)? as usize)
            .ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let mut bootstrap_index = bootstrap_methods[self.read_u2(cpi + 1)? as usize];
        let bootstrap = into_handle(self.read_const(self.read_u2(bootstrap_index)? as usize, string_cache, bootstrap_methods)?);
        let bsm_argc = self.read_u2(bootstrap_index + 2)? as usize;
        let mut bsm_argv = Vec::with_capacity(bsm_argc);
        bootstrap_index += 4;
        for _ in 0..bsm_argc {
            bsm_argv.push(self.read_const(self.read_u2(bootstrap_index)? as usize, string_cache, bootstrap_methods)?);
            bootstrap_index += 2;
        }
        let nat = self.read_name_and_type(cpi + 3, string_cache)?;
        let name = nat.0;
        let desc = nat.1;
        Ok(ConstantDynamic {name, desc, bootstrap, args: bsm_argv})
    }
    fn read_annotation_values(&self, mut at: usize, named: bool, mut vis: &mut Option<&mut dyn AnnotationVisitor>,
                              string_cache: &mut HashMap<usize, Rc<str>>, bootstrap_methods: &[usize]) -> Result<usize> {
        let num = self.read_u2(at)?;
        at += 2;
        if named {
            for _ in 0..num {
                let name = self.read_utf8(at, string_cache)?;
                at = self.read_annotation_value(at + 2, name, &mut vis, string_cache, bootstrap_methods)?;
            }
        } else {
            for _ in 0..num {
                at = self.read_annotation_value(at, Rc::from(""), &mut vis, string_cache, bootstrap_methods)?;
            }
        }
        vis.as_mut().map(|v| v.visit_end());
        return Ok(at);
    }
    fn read_annotation_value(&self, mut at: usize, name: Rc<str>, vis: &mut Option<&mut dyn AnnotationVisitor>,
                             string_cache: &mut HashMap<usize, Rc<str>>, bootstrap_methods: &[usize]) -> Result<usize> {
        let tag = self.read_u1(at)?;
        if vis.is_none() {
            match tag as char {
                'e' => {
                    return Ok(at + 5);
                },
                '@' => {
                    return self.read_annotation_values(at + 3, true, vis, string_cache, bootstrap_methods);
                },
                '[' => {
                    return self.read_annotation_values(at + 1, false, vis, string_cache, bootstrap_methods);
                },
                _ => {
                    return Ok(at + 3);
                }
            }
        } else if let &mut Some(ref mut vis) = vis {
            at += 1;
            match tag as char {
                'I' | 'F' | 'D' | 'J' | 'B' | 'C' | 'S' | 'Z' => {
                    let csta = self.read_u2(at)? as usize;
                    let cst = self.read_const(csta, string_cache, bootstrap_methods)?;
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
                    let string = self.read_utf8(at, string_cache)?;
                    vis.visit_primitive(name,
                                        AnnotationPrimitive::String(string));
                    return Ok(at + 2);
                },
                'e' => {
                    let value = self.read_utf8(at + 2, string_cache)?;
                    let desc = self.read_utf8(at, string_cache)?;
                    vis.visit_enum(name, desc, value);
                    return Ok(at + 4);
                },
                'c' => {
                    let desc = self.read_utf8(at, string_cache)?;
                    vis.visit_primitive(name,
                                        AnnotationPrimitive::Type(Type::new(desc)));
                    return Ok(at + 2);
                }
                '@' => {
                    let desc = self.read_utf8(at, string_cache)?;
                    let mut nv = vis.visit_annotation(name, desc);
                    at = self.read_annotation_values(at + 2, true, &mut nv, string_cache, bootstrap_methods)?;
                },
                '[' => {
                    let mut nv = vis.visit_array(name);
                    return self.read_annotation_values(at, false, &mut nv, string_cache, bootstrap_methods);
                },
                _ => {
                    return Err(ClassDecodeError::UnrecognizedAnnotationTag);
                }
            }
            return Ok(at);

        }
        unreachable!();
    }
    fn read_annotation_target(&self, mut at: usize) -> Result<(usize, TypeRef, Vec<TypePathEntry>)> {
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
    fn read_type_path(&self, mut at: usize) -> Result<(usize, Vec<TypePathEntry>)> {
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

    fn read_const(&self, item: usize, string_cache: &mut HashMap<usize, Rc<str>>, bootstrap_methods: &[usize]) -> Result<ClassConstant> {
        let at = self.const_pool.get(item).ok_or(ClassDecodeError::ConstantPoolIndexOutOfBounds)?;
        let discr = self.read_u1(at + 0)?;
        match discr {
            constant_pool_entry::INT => {
                self.read_i4(at + 1).map(|c| ClassConstant::Integer(c))
            },
            constant_pool_entry::FLOAT => {
                self.read_f4(at + 1).map(|c| ClassConstant::Float(c))
            },
            constant_pool_entry::LONG => {
                self.read_i8(at + 1).map(|c| ClassConstant::Long(c))
            },
            constant_pool_entry::DOUBLE => {
                self.read_f8(at + 1).map(|c| ClassConstant::Double(c))
            },
            constant_pool_entry::STR => {
                self.read_utf8(at + 1, string_cache).map(|c| ClassConstant::String(c))
            },
            constant_pool_entry::CLASS => {
                self.read_utf8(at + 1, string_cache)
                    .map(|c| Type::new_object_type(c))
                    .map(|c| ClassConstant::Class(c))
            },
            constant_pool_entry::MTYPE => {
                self.read_utf8(at + 1, string_cache)
                    .map(|c| Type::new(c))
                    .map(|c| ClassConstant::MethodType(c))
            },
            constant_pool_entry::HANDLE => {
                let kind = self.read_u1(at + 1)?;
                let info = self.read_method_ref(at + 2, string_cache)?;
                Ok(ClassConstant::MethodHandle(
                    Handle::new(kind,info.0, (info.1).0, (info.1).1)))
            },
            constant_pool_entry::CONDY => {
                Ok(ClassConstant::ConstantDynamic(self.read_indy(at + 1, bootstrap_methods, string_cache)?))
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
    INSN_CLASS_TABLE.get(insn as usize).map(|c| *c)
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
