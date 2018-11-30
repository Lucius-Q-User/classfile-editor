#![feature(core_intrinsics)]
pub mod opcodes;
mod somewhat_unique_id;
use crate::somewhat_unique_id::UID;
use std::borrow::Borrow;
use std::rc::Rc;

#[cfg(feature = "tree")]
pub mod tree;
#[cfg(feature = "signature")]
pub mod signature;
pub mod reader;
use bitflags::*;


bitflags! {
    pub struct ClassAccess: u32 {
        const ACC_PUBLIC = 0x1;
        const ACC_FINAL = 0x10;
        const ACC_SUPER = 0x20;
        const ACC_INTERFACE = 0x200;
        const ACC_ABSTRACT = 0x400;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_ANNOTATION = 0x2000;
        const ACC_ENUM = 0x4000;
        const ACC_MODULE = 0x8000;
        const ACC_PSEUDO_DEPRECATED = 0x20000;
    }
}

bitflags! {
    pub struct InnerClassAccess: u32 {
        const ACC_PUBLIC = 0x1;
        const ACC_PRIVATE = 0x2;
        const ACC_PROTECTED = 0x4;
        const ACC_STATIC = 0x8;
        const ACC_FINAL = 0x10;
        const ACC_INTERFACE = 0x200;
        const ACC_ABSTRACT = 0x400;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_ANNOTATION = 0x2000;
        const ACC_ENUM = 0x4000;
    }
}

bitflags! {
    pub struct FieldAccess: u32 {
        const ACC_PUBLIC = 0x1;
        const ACC_PRIVATE = 0x2;
        const ACC_PROTECTED = 0x4;
        const ACC_STATIC = 0x8;
        const ACC_FINAL = 0x10;
        const ACC_VOLATILE = 0x40;
        const ACC_TRANSIENT = 0x80;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_ENUM = 0x4000;
        const ACC_PSEUDO_DEPRECATED = 0x20000;
    }
}

bitflags! {
    pub struct MethodAccess: u32 {
        const ACC_PUBLIC = 0x1;
        const ACC_PRIVATE = 0x2;
        const ACC_PROTECTED = 0x4;
        const ACC_STATIC = 0x8;
        const ACC_FINAL = 0x10;
        const ACC_SYNCHRONIZED = 0x20;
        const ACC_BRIDGE = 0x40;
        const ACC_VARARGS = 0x80;
        const ACC_NATIVE = 0x100;
        const ACC_ABSTRACT = 0x400;
        const ACC_STRICT = 0x800;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_PSEUDO_DEPRECATED = 0x20000;
    }
}

bitflags! {
    pub struct ParameterAccess: u32 {
        const ACC_FINAL = 0x10;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

bitflags! {
    pub struct ModuleFlags: u32 {
        const ACC_OPEN = 0x20;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

bitflags! {
    pub struct RequireFlags: u32 {
        const ACC_TRANSITIVE = 0x20;
        const ACC_STATIC_PHASE = 0x40;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

bitflags! {
    pub struct ExportFlags: u32 {
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Handle {
    tag: u8,
    owner: Rc<str>,
    name: Rc<str>,
    desc: Rc<str>
}
#[derive(Debug, Clone, PartialEq)]
pub struct ConstantDynamic {
    name: Rc<str>,
    desc: Rc<str>,
    bootstrap: Handle,
    args: Vec<ClassConstant>
}

impl Handle {
    fn new(tag: u8, owner: Rc<str>, name: Rc<str>, desc: Rc<str>) -> Handle {
        Handle {
            tag, owner, name, desc
        }
    }
}
///An abstract token used to represent a position in code.
///The only useful operation is the equality comparison.
#[derive(PartialEq, Eq, Hash, Clone, Debug, Copy)]
pub struct Label {
    data: UID
}
#[derive(Clone, Debug, Copy, PartialEq)]
pub struct LocalVariableSpan {
    pub end: Label,
    pub index: u16,
    pub start: Label,
}
impl LocalVariableSpan {
    pub fn new(start: Label, end: Label, index: u16) -> LocalVariableSpan {
        LocalVariableSpan {
            start, end, index
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FrameMode {
    Full,
    Append,
    Chop,
    Same,
    Same1
}

#[derive(Debug, Clone, PartialEq)]
pub enum FrameItem {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Class(Rc<str>),
    Uninitialized(Label),
    Void //Only used internally, dont use
}

impl Label {
    fn new() -> Label {
        let data = UID::new();
        Label {data}
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type {
    desc: Rc<str>
}

impl Type {
    pub fn new<T>(desc: T) -> Type where T: Into<Rc<str>> {
        Type {
            desc: desc.into()
        }
    }
    pub fn new_object_type<T>(desc: T) -> Type where T: Into<Rc<str>> {
        let c = desc.into();
        let ds = if c.as_bytes()[0] as char == '[' {
            c
        } else {
            Rc::from(format!("L{};", c))
        };
        Type {
            desc: ds
        }
    }
    pub fn get_descriptor(&self) -> &str {
        self.desc.borrow()
    }
    pub fn dimensions(&self) -> usize {
        let ds: &str = self.desc.borrow();
        if ds.as_bytes()[0] as char == '[' {
            1 + Type::new(&ds[1..ds.len()]).dimensions()
        } else {
            0
        }
    }
    pub fn element_type(&self) -> Type {
        let ds: &str = self.desc.borrow();
        for i in 0..ds.len() {
            if ds.as_bytes()[i] as char != '[' {
                let ss: &str = &ds[i..ds.len()];
                return Type::new(ss)
            }
        }
        panic!();
    }
    pub fn size(&self) -> usize {
        let ds: &str = self.desc.borrow();
        let lt = ds.as_bytes()[0] as char;
        match lt {
            'J' | 'D' => 2,
            'V' => 0,
            _ => 1
        }
    }
    pub fn method_type(ret: &Type, args: &[&Type]) -> Type {
        let mut tm1 = String::new();
        for s in args {
            tm1.push_str(s.get_descriptor());
        }
        Type::new(format!("({}){}", tm1, ret.get_descriptor()))
    }
    pub fn return_type(&self) -> Type {
        let ds: &str = self.desc.borrow();
        for (i, c) in ds.char_indices() {
            if c == ')' {
                return Type::new(&ds[i + 1..ds.len()]);
            }
        }
        panic!();
    }
    pub fn argument_and_return_size(&self) -> (usize, usize) {
        let arg = self.argument_types().iter().fold(1, |a, e| a + e.size());
        let ret = self.return_type().size();
        (arg, ret)
    }
    pub fn argument_types(&self) -> Vec<Type> {
        let ds: &str = self.desc.borrow();
        let mut args = Vec::new();
        let mut parsing_array = false;
        let mut parsing_object = false;
        let mut item_start = 0;
        for (i, c) in ds.char_indices() {
            match c {
                '(' => {
                },
                ')' => {
                    break;
                },
                '[' => {
                    if !parsing_array {
                        item_start = i;
                    }
                    parsing_array = true;
                },
                'L' => {
                    if !(parsing_array || parsing_object) {
                        item_start = i;
                    }
                    parsing_object = true;
                },
                ';' => {
                    args.push(Type::new(&ds[item_start..=i]));
                    parsing_array = false;
                    parsing_object = false;
                },
                'V' | 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => {
                    if !parsing_object {
                        if !parsing_array {
                            args.push(Type::new(&ds[i..=i]));
                        } else {
                            args.push(Type::new(&ds[item_start..=i]));
                        }
                        parsing_array = false;
                    }
                },
                _ => {
                }
            }
        }
        args
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ClassConstant {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Class(Type),
    String(Rc<str>),
    MethodType(Type),
    MethodHandle(Handle),
    ConstantDynamic(ConstantDynamic)
}
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationPrimitive {
    Byte(i8),
    Boolean(bool),
    Character(i16),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(Rc<str>),
    Type(Type)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypePathEntry {
    ArrayElement,
    InnerType,
    WildcardBound,
    TypeArgument(u8)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeRef {
    ClassTypeParameter(u8),
    ClassExtends(u16),
    ClassTypeParamenterBound {parameter: u8, bound: u8},
    FieldDeclaration,
    MethodTypeParameter(u8),
    MethodTypeParameterBound {parameter: u8, bound: u8},
    MethodReturnType,
    MethodRecieverType,
    MethodFormalParameter(u8), //Due to javac being javac the value can be not what you expect
    ThrowsClause(u16),
    LocalVariable,
    ResourceVariable,
    Instanceof,
    New,
    MethodReference,
    ContructorReference,
    Cast(u8),
    GenericConstructorInvocation(u8),
    GenericMethodInvocation(u8),
    GenericConstructorReference(u8),
    GenericMethodReference(u8)
}

///Visits a Java annotation. The methods will be called in the following order:
/// (`visit_primitive` | `visit_array` | `visit_annotation` | `visit_enum`) `visit_end`
/// If used to visit an array, an empty string must be passed as `name` argument
/// If `get_wrapped_visitor` returns `Some`, unimplemented methods will be forwarded to that visitor
pub trait AnnotationVisitor {
    ///Returns a visitor that the unimplemented methods will be forwarded to.
    fn get_wrapped_visitor(&self) -> Option<&mut dyn AnnotationVisitor> {
        None
    }
    ///Visits a primitive value. Unlike in ASM, this can't be used to visit arrays.
    fn visit_primitive(&mut self, name: Rc<str>, value: AnnotationPrimitive) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_primitive(name, value);
        }
    }
    fn visit_enum(&mut self, name: Rc<str>, desc: Rc<str>, value: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_enum(name, desc, value);
        }
    }
    fn visit_annotation(&mut self, name: Rc<str>, desc: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotation(name, desc)
        } else {
            None
        }
    }
    ///Returns a visitor to visit the array members
    fn visit_array(&mut self, name: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_array(name)
        } else {
            None
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
}
pub trait FieldVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn FieldVisitor> {
        None
    }
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotation(desc, visible)
        } else {
            None
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_annotation(type_ref, type_path, desc, visible)
        } else {
            None
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
}
pub trait MethodVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn MethodVisitor> {
        None
    }
    fn visit_tableswitch_insn(&mut self, min: i32, max: i32, dflt: Label, labels: Vec<Label>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_tableswitch_insn(min, max, dflt, labels);
        }
    }
    fn visit_frame(&mut self, mode: FrameMode, n_locals: u16, locals: &[FrameItem], n_stack: u16, stack: &[FrameItem]) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_frame(mode, n_locals, locals, n_stack, stack);
        }
    }
    fn visit_insn(&mut self, opcode: u8) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_insn(opcode);
        }
    }
    fn visit_multianew_insn(&mut self, class: Rc<str>, dimensions: u8) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_multianew_insn(class, dimensions);
        }
    }
    fn visit_type_insn(&mut self, opcode: u8, class: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_insn(opcode, class);
        }
    }
    fn visit_ldc_insn(&mut self, cst: ClassConstant) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_ldc_insn(cst);
        }
    }
    fn visit_lookupswitch_insn(&mut self, dflt: Label, pairs: Vec<(i32, Label)>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_lookupswitch_insn(dflt, pairs);
        }
    }
    fn visit_indy_insn(&mut self, name: Rc<str>, desc: Rc<str>, bsm: Handle, args: Vec<ClassConstant>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_indy_insn(name, desc, bsm, args);
        }
    }
    fn visit_field_method_insn(&mut self, opcode: u8, owner: Rc<str>, name: Rc<str>, desc: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_field_method_insn(opcode, owner, name, desc);
        }
    }
    fn visit_jump_insn(&mut self, opcode: u8, label: Label) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_jump_insn(opcode, label);
        }
    }
    fn visit_iinc_insn(&mut self, var: u16, by: i16) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_iinc_insn(var, by);
        }
    }
    fn visit_var_insn(&mut self, opcode: u8, var: u16) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_var_insn(opcode, var);
        }
    }
    fn visit_int_insn(&mut self, opcode: u8, operand: i32) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_int_insn(opcode, operand);
        }
    }
    fn visit_local_var(&mut self, name: Rc<str>, desc: Rc<str>, sig: Option<Rc<str>>, span: LocalVariableSpan) {
       if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_local_var(name, desc, sig, span);
        }
    }
    fn visit_maxs(&mut self, max_stack: u16, max_local: u16) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_maxs(max_stack, max_local);
        }
    }
    fn visit_label(&mut self, label: Label) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_label(label);
        }
    }
    fn visit_line_number(&mut self, line: u16, label: Label) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_line_number(line, label);
        }
    }
    fn visit_parameter(&mut self, name: Rc<str>, access: ParameterAccess) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_parameter(name, access);
        }
    }
    fn visit_annotation_default(&mut self) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotation_default()
        } else {
            None
        }
    }
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotation(desc, visible)
        } else {
            None
        }
    }
    fn visit_insn_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_insn_annotation(type_ref, type_path, desc, visible)
        } else {
            None
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_annotation(type_ref, type_path, desc, visible)
        } else {
            None
        }
    }
    fn visit_trycatch_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_trycatch_annotation(type_ref, type_path, desc, visible)
        } else {
            None
        }
    }
    fn visit_local_variable_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, spans: Vec<LocalVariableSpan>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_local_variable_annotation(type_ref, type_path, spans, desc, visible)
        } else {
            None
        }
    }
    fn visit_parameter_annotation(&mut self, parameter: u8, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_parameter_annotation(parameter, desc, visible)
        } else {
            None
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
    fn visit_code(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_code();
        }
    }
    fn visit_try_catch(&mut self, start: Label, end: Label, handler: Label, catch_type: Option<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_try_catch(start, end, handler, catch_type);
        }
    }
    fn visit_annotable_parameter_count(&mut self, count: u8, visible: bool) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotable_parameter_count(count, visible);
        }
    }
}
pub trait ModuleVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn ModuleVisitor> {
        None
    }
    fn visit_main_class(&mut self, name: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_main_class(name);
        }
    }
    fn visit_package(&mut self, pkg: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_package(pkg);
        }
    }
    fn visit_require(&mut self, module: Rc<str>, flags: RequireFlags, version: Option<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_require(module, flags, version);
        }
    }
    fn visit_export(&mut self, pkg: Rc<str>, flags: ExportFlags, modules: Vec<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_export(pkg, flags, modules);
        }
    }
    fn visit_open(&mut self, pkg: Rc<str>, flags: ExportFlags, modules: Vec<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_open(pkg, flags, modules);
        }
    }
    fn visit_provide(&mut self, service: Rc<str>, providers: Vec<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_provide(service, providers);
        }
    }
    fn visit_use(&mut self, service: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_use(service);
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
}

pub trait ClassVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn ClassVisitor> {
        None
    }
    fn visit_header(&mut self, version_minor:u16, version_major:u16, access:ClassAccess, name: Rc<str>,
                   signature: Option<Rc<str>>, super_name: Option<Rc<str>>,
                   interfaces: Vec<Rc<str>>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_header(version_minor, version_major, access, name, signature, super_name, interfaces);
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
    fn visit_source(&mut self, source: Option<Rc<str>>, debug: Option<String>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_source(source, debug);
        }
    }
    fn visit_outer_class(&mut self, owner: Rc<str>, method: Option<(Rc<str>, Rc<str>)>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_outer_class(owner, method);
        }
    }
    fn visit_nest_host(&mut self, name: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_nest_host(name);
        }
    }
    fn visit_nest_member(&mut self, name: Rc<str>) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_nest_member(name);
        }
    }
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_annotation(desc, visible)
        } else {
            None
        }
    }
    fn visit_module(&mut self, name: Rc<str>, flags: ModuleFlags, version: Option<Rc<str>>) -> Option<&mut dyn ModuleVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_module(name, flags, version)
        } else {
            None
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_annotation(type_ref, type_path, desc, visible)
        } else {
            None
        }
    }
    fn visit_inner_class(&mut self, inner_name: Rc<str>, outer_name: Option<Rc<str>>, simple_inner_name: Option<Rc<str>>, access: InnerClassAccess) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_inner_class(inner_name, outer_name, simple_inner_name, access);
        }
    }
    fn visit_field(&mut self, access: FieldAccess, name: Rc<str>, desc: Rc<str>, signature: Option<Rc<str>>, value: Option<ClassConstant>) -> Option<&mut dyn FieldVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_field(access, name, desc, signature, value)
        } else {
            None
        }
    }
    fn visit_method(&mut self, access: MethodAccess, name: Rc<str>, desc: Rc<str>, signature: Option<Rc<str>>, exceptions: Vec<Rc<str>>) -> Option<&mut dyn MethodVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_method(access, name, desc, signature, exceptions)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! function {
        () => {{
            fn f() {}
            fn type_name_of<T>(_: T) -> &'static str {
                use core;
                unsafe { core::intrinsics::type_name::<T>() }
            }
            let name = type_name_of(f);
            &name[6..name.len() - 4]
        }}
    }
    use std::io::Read;
    use std::fs::File;
    use super::*;
    use crate::reader::ClassReaderFlags;
    struct SimpleLogger4;
    static mut AV: AnnVis = AnnVis{};
    struct MethVis;
    impl MethodVisitor for MethVis {
        fn visit_label(&mut self, label: Label) {
            eprintln!("{}: {:?}", function!(), label);
        }
        fn visit_insn(&mut self, opcode: u8) {
            eprintln!("{}: {:?}", function!(), opcode_to_name(opcode));
        }
        fn visit_var_insn(&mut self, opcode: u8, var: u16) {
            eprintln!("{}: {:?}, {:?}", function!(), opcode_to_name(opcode), var);
        }
        fn visit_jump_insn(&mut self, opcode: u8, label: Label) {
            eprintln!("{}: {:?}, {:?}", function!(), opcode_to_name(opcode), label);
        }
        fn visit_multianew_insn(&mut self, class: Rc<str>, dimensions: u8) {
            eprintln!("{}: {:?}, {:?}", function!(), class, dimensions);
        }
        fn visit_iinc_insn(&mut self, var: u16, by: i16) {
            eprintln!("{}: {:?}, {:?}", function!(), var, by);
        }
        fn visit_int_insn(&mut self, opcode: u8, operand: i32) {
            eprintln!("{}: {:?}, {:?}", function!(), opcode_to_name(opcode), operand);
        }
        fn visit_type_insn(&mut self, opcode: u8, class: Rc<str>) {
            eprintln!("{}: {:?}, {:?}", function!(), opcode_to_name(opcode), class);
        }
        fn visit_lookupswitch_insn(&mut self, dflt: Label, pairs: Vec<(i32, Label)>) {
            eprintln!("{}: {:?}, {:?}", function!(), dflt, pairs);
        }
        fn visit_local_var(&mut self, name: Rc<str>, desc: Rc<str>, sig: Option<Rc<str>>, span: LocalVariableSpan) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), name, desc, sig, span);
        }
        fn visit_frame(&mut self, mode: FrameMode, n_locals: u16, locals: &[FrameItem], n_stack: u16, stack: &[FrameItem]) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}, {:?}", function!(), mode, n_locals, locals, n_stack, stack);
        }
        fn visit_indy_insn(&mut self, name: Rc<str>, desc: Rc<str>, bsm: Handle, args: Vec<ClassConstant>) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), name, desc, bsm, args);
        }
        fn visit_tableswitch_insn(&mut self, min: i32, max: i32, dflt: Label, labels: Vec<Label>) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), min, max, dflt, labels);
        }
        fn visit_line_number(&mut self, line: u16, label: Label) {
            eprintln!("{}: {:?}, {:?}", function!(), line, label);
        }
        fn visit_parameter(&mut self, name: Rc<str>, access: ParameterAccess) {
            eprintln!("{}: {:?}, {:?}", function!(), name, access);
        }
        fn visit_annotation_default(&mut self) -> Option<&mut dyn AnnotationVisitor> {
            unsafe {Some(&mut AV)}
        }
        fn visit_ldc_insn(&mut self, cst: ClassConstant) {
            eprintln!("{}: {:?}", function!(), cst);
        }
        fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}", function!(), desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_insn_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_trycatch_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_local_variable_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, spans: Vec<LocalVariableSpan>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
                eprintln!("{}: {:?}, {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, spans, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_field_method_insn(&mut self, opcode: u8, owner: Rc<str>, name: Rc<str>, desc: Rc<str>) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), opcode_to_name(opcode), owner, name, desc);
        }
        fn visit_try_catch(&mut self, start: Label, end: Label, handler: Label, catch_type: Option<Rc<str>>) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), start, end, handler, catch_type);
        }
    }
    static mut MV: MethVis = MethVis{};
    impl ClassVisitor for SimpleLogger4 {
        fn visit_inner_class(&mut self, inner_name: Rc<str>, outer_name: Option<Rc<str>>, simple_inner_name: Option<Rc<str>>, access: InnerClassAccess) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), inner_name, outer_name, simple_inner_name, access);

        }
        fn visit_header(&mut self, version_minor:u16, version_major:u16, access:ClassAccess, name: Rc<str>,
                        signature: Option<Rc<str>>, super_name: Option<Rc<str>>,
                        interfaces: Vec<Rc<str>>) {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}", function!(), version_minor, version_major, access, name, signature, interfaces, super_name);
        }
        fn visit_outer_class(&mut self, owner: Rc<str>, method: Option<(Rc<str>, Rc<str>)>) {
            eprintln!("{}: {:?}, {:?}", function!(), owner, method);
        }
        fn visit_source(&mut self, source: Option<Rc<str>>, debug: Option<String>) {
            eprintln!("{}: {:?}, {:?}", function!(), source, debug);
        }
        fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}", function!(), desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_field(&mut self, access: FieldAccess, name: Rc<str>, desc: Rc<str>, signature: Option<Rc<str>>, value: Option<ClassConstant>) -> Option<&mut dyn FieldVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}, {:?}", function!(), access, name, desc, signature, value);
            unsafe {Some(&mut FV)}

        }
        fn visit_method(&mut self, access: MethodAccess, name: Rc<str>, desc: Rc<str>, signature: Option<Rc<str>>, exceptions: Vec<Rc<str>>) -> Option<&mut dyn MethodVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}, {:?}", function!(), access, name, desc, signature, exceptions);
            unsafe {Some(&mut MV)}
        }
        fn visit_end(&mut self) {
            eprintln!("{}", function!());
        }
    }
    struct AnnVis;
    impl AnnotationVisitor for AnnVis {
        fn visit_primitive(&mut self, name: Rc<str>, value: AnnotationPrimitive) {
            eprintln!("{}: {:?}, {:?}", function!(), name, value);
        }
        fn visit_end(&mut self) {
            eprintln!("{}", function!());
        }
        fn visit_annotation(&mut self, name: Rc<str>, desc: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}", function!(), name, desc);
            return unsafe { Some(&mut AV) };
        }
        fn visit_array(&mut self, name: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}", function!(), name);
            return unsafe { Some(&mut AV) };
        }
        fn visit_enum(&mut self, name: Rc<str>, desc: Rc<str>, value: Rc<str>) {
            eprintln!("{}: {:?}, {:?}, {:?}", function!(), name, desc, value);
        }
    }
    struct FieldVis;
    static mut FV: FieldVis= FieldVis {};
    impl FieldVisitor for FieldVis {
        fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}", function!(), desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: Vec<TypePathEntry>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
            eprintln!("{}: {:?}, {:?}, {:?}, {:?}", function!(), type_ref, type_path, desc, visible);
            return unsafe { Some(&mut AV) };
        }
        fn visit_end(&mut self) {
            eprintln!("{}", function!());
        }
    }
    //#[test]
    fn it_works3() {
        let mut bytes = Vec::new();
        File::open("/Users/Alice/Desktop/eclipse-workspace/class2json/target/classes/class2json/parse/Main.class").unwrap().read_to_end(&mut bytes).unwrap();
        let reader = super::reader::ClassReader::new(&bytes);
        let mut visitor = SimpleLogger4 {};
        reader.accept(&mut visitor, ClassReaderFlags::empty()).unwrap();
    }
    fn opcode_to_name(opcode: u8) -> &'static str {
        match opcode {
            0 => {"NOP"},
            1 => {"ACONST_NULL"},
            2 => {"ICONST_M1"},
            3 => {"ICONST_0"},
            4 => {"ICONST_1"},
            5 => {"ICONST_2"},
            6 => {"ICONST_3"},
            7 => {"ICONST_4"},
            8 => {"ICONST_5"},
            9 => {"LCONST_0"},
            10 => {"LCONST_1"},
            11 => {"FCONST_0"},
            12 => {"FCONST_1"},
            13 => {"FCONST_2"},
            14 => {"DCONST_0"},
            15 => {"DCONST_1"},
            16 => {"BIPUSH"},
            17 => {"SIPUSH"},
            18 => {"LDC"},
            21 => {"ILOAD"},
            22 => {"LLOAD"},
            23 => {"FLOAD"},
            24 => {"DLOAD"},
            25 => {"ALOAD"},
            46 => {"IALOAD"},
            47 => {"LALOAD"},
            48 => {"FALOAD"},
            49 => {"DALOAD"},
            50 => {"AALOAD"},
            51 => {"BALOAD"},
            52 => {"CALOAD"},
            53 => {"SALOAD"},
            54 => {"ISTORE"},
            55 => {"LSTORE"},
            56 => {"FSTORE"},
            57 => {"DSTORE"},
            58 => {"ASTORE"},
            79 => {"IASTORE"},
            80 => {"LASTORE"},
            81 => {"FASTORE"},
            82 => {"DASTORE"},
            83 => {"AASTORE"},
            84 => {"BASTORE"},
            85 => {"CASTORE"},
            86 => {"SASTORE"},
            87 => {"POP"},
            88 => {"POP2"},
            89 => {"DUP"},
            90 => {"DUP_X1"},
            91 => {"DUP_X2"},
            92 => {"DUP2"},
            93 => {"DUP2_X1"},
            94 => {"DUP2_X2"},
            95 => {"SWAP"},
            96 => {"IADD"},
            97 => {"LADD"},
            98 => {"FADD"},
            99 => {"DADD"},
            100 => {"ISUB"},
            101 => {"LSUB"},
            102 => {"FSUB"},
            103 => {"DSUB"},
            104 => {"IMUL"},
            105 => {"LMUL"},
            106 => {"FMUL"},
            107 => {"DMUL"},
            108 => {"IDIV"},
            109 => {"LDIV"},
            110 => {"FDIV"},
            111 => {"DDIV"},
            112 => {"IREM"},
            113 => {"LREM"},
            114 => {"FREM"},
            115 => {"DREM"},
            116 => {"INEG"},
            117 => {"LNEG"},
            118 => {"FNEG"},
            119 => {"DNEG"},
            120 => {"ISHL"},
            121 => {"LSHL"},
            122 => {"ISHR"},
            123 => {"LSHR"},
            124 => {"IUSHR"},
            125 => {"LUSHR"},
            126 => {"IAND"},
            127 => {"LAND"},
            128 => {"IOR"},
            129 => {"LOR"},
            130 => {"IXOR"},
            131 => {"LXOR"},
            132 => {"IINC"},
            133 => {"I2L"},
            134 => {"I2F"},
            135 => {"I2D"},
            136 => {"L2I"},
            137 => {"L2F"},
            138 => {"L2D"},
            139 => {"F2I"},
            140 => {"F2L"},
            141 => {"F2D"},
            142 => {"D2I"},
            143 => {"D2L"},
            144 => {"D2F"},
            145 => {"I2B"},
            146 => {"I2C"},
            147 => {"I2S"},
            148 => {"LCMP"},
            149 => {"FCMPL"},
            150 => {"FCMPG"},
            151 => {"DCMPL"},
            152 => {"DCMPG"},
            153 => {"IFEQ"},
            154 => {"IFNE"},
            155 => {"IFLT"},
            156 => {"IFGE"},
            157 => {"IFGT"},
            158 => {"IFLE"},
            159 => {"IF_ICMPEQ"},
            160 => {"IF_ICMPNE"},
            161 => {"IF_ICMPLT"},
            162 => {"IF_ICMPGE"},
            163 => {"IF_ICMPGT"},
            164 => {"IF_ICMPLE"},
            165 => {"IF_ACMPEQ"},
            166 => {"IF_ACMPNE"},
            167 => {"GOTO"},
            168 => {"JSR"},
            169 => {"RET"},
            170 => {"TABLESWITCH"},
            171 => {"LOOKUPSWITCH"},
            172 => {"IRETURN"},
            173 => {"LRETURN"},
            174 => {"FRETURN"},
            175 => {"DRETURN"},
            176 => {"ARETURN"},
            177 => {"RETURN"},
            178 => {"GETSTATIC"},
            179 => {"PUTSTATIC"},
            180 => {"GETFIELD"},
            181 => {"PUTFIELD"},
            182 => {"INVOKEVIRTUAL"},
            183 => {"INVOKESPECIAL"},
            184 => {"INVOKESTATIC"},
            185 => {"INVOKEINTERFACE"},
            186 => {"INVOKEDYNAMIC"},
            187 => {"NEW"},
            188 => {"NEWARRAY"},
            189 => {"ANEWARRAY"},
            190 => {"ARRAYLENGTH"},
            191 => {"ATHROW"},
            192 => {"CHECKCAST"},
            193 => {"INSTANCEOF"},
            194 => {"MONITORENTER"},
            195 => {"MONITOREXIT"},
            197 => {"MULTIANEWARRAY"},
            198 => {"IFNULL"},
            199 => {"IFNONNULL"},
            _ => panic!()
        }
    }
}
