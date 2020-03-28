use super::{ClassVisitor, MethodVisitor, AnnotationVisitor, FieldVisitor, ModuleVisitor, RecordComponentVisitor};
use super::{ParameterAccess, ClassAccess, MethodAccess, FieldAccess, InnerClassAccess};
use super::{ExportFlags, RequireFlags, ModuleFlags};
use super::{ClassConstant, Label, Handle, TypePath, TypeRef, ClassVersion};
use super::{FrameItem, FrameMode, AnnotationPrimitive, LocalVariableSpan, NameAndType};
use std::{
    rc::Rc,
    vec::Vec,
    collections::HashMap,
    cmp::max
};

#[derive(Debug, Clone, PartialEq)]
pub struct RecordComponentNode {
    pub id: NameAndType,
    pub invisible_annotations: Vec<AnnotationNode>,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub signature: Option<Rc<str>>,
    pub visible_annotations: Vec<AnnotationNode>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>
}

impl RecordComponentNode {
    pub fn new(id: NameAndType, signature: Option<Rc<str>>) -> RecordComponentNode {
        RecordComponentNode {
            id, signature,
            invisible_annotations: Vec::new(),
            invisible_type_annotations: Vec::new(),
            visible_annotations: Vec::new(),
            visible_type_annotations: Vec::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn ClassVisitor) {
        if let Some(rcv) = vis.visit_record_component(self.id.clone(), self.signature.clone()) {
            for ann in &self.visible_annotations {
                if let Some(vis) = rcv.visit_annotation(ann.desc.clone(), true) {
                    ann.accept(vis);
                }
            }
            for ann in &self.invisible_annotations {
                if let Some(vis) = rcv.visit_annotation(ann.desc.clone(), false) {
                    ann.accept(vis);
                }
            }
            for ann in &self.invisible_type_annotations {
                if let Some(vis) = rcv.visit_type_annotation(ann.type_ref, ann.type_path.clone(), ann.annotation.desc.clone(), false) {
                    ann.annotation.accept(vis);
                }
            }
            for ann in &self.visible_type_annotations {
                if let Some(vis) = rcv.visit_type_annotation(ann.type_ref, ann.type_path.clone(), ann.annotation.desc.clone(), true) {
                    ann.annotation.accept(vis);
                }
            }
            rcv.visit_end();
        }
    }
}
impl RecordComponentVisitor for RecordComponentNode {
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = AnnotationNode::new(desc);
        if visible {
            self.visible_annotations.push(at);
            self.visible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_annotations.push(at);
            self.invisible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool)-> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.visible_type_annotations.push(at);
            self.visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_type_annotations.push(at);
            self.invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldNode {
    pub access: FieldAccess,
    pub id: NameAndType,
    pub invisible_annotations: Vec<AnnotationNode>,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub signature: Option<Rc<str>>,
    pub value: Option<ClassConstant>,
    pub visible_annotations: Vec<AnnotationNode>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>
}

impl FieldNode {
    pub fn new(access: FieldAccess, id: NameAndType, signature: Option<Rc<str>>, value: Option<ClassConstant>) -> FieldNode {
        FieldNode {
            access, id, signature, value,
            invisible_annotations: Vec::new(),
            invisible_type_annotations: Vec::new(),
            visible_annotations: Vec::new(),
            visible_type_annotations: Vec::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn ClassVisitor) {
        if let Some(fv) = vis.visit_field(self.access, self.id.clone(), self.signature.clone(), self.value.clone()) {
            for ann in &self.visible_annotations {
                if let Some(vis) = fv.visit_annotation(ann.desc.clone(), true) {
                    ann.accept(vis);
                }
            }
            for ann in &self.invisible_annotations {
                if let Some(vis) = fv.visit_annotation(ann.desc.clone(), false) {
                    ann.accept(vis);
                }
            }
            for ann in &self.invisible_type_annotations {
                if let Some(vis) = fv.visit_type_annotation(ann.type_ref, ann.type_path.clone(), ann.annotation.desc.clone(), false) {
                    ann.annotation.accept(vis);
                }
            }
            for ann in &self.visible_type_annotations {
                if let Some(vis) = fv.visit_type_annotation(ann.type_ref, ann.type_path.clone(), ann.annotation.desc.clone(), true) {
                    ann.annotation.accept(vis);
                }
            }
            fv.visit_end();
        }
    }
}
impl FieldVisitor for FieldNode {
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = AnnotationNode::new(desc);
        if visible {
            self.visible_annotations.push(at);
            self.visible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_annotations.push(at);
            self.invisible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool)-> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.visible_type_annotations.push(at);
            self.visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_type_annotations.push(at);
            self.invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InnerClassNode {
    pub access: InnerClassAccess,
    pub inner_name: Rc<str>,
    pub name: Option<Rc<str>>,
    pub outer_name: Option<Rc<str>>,
}

impl InnerClassNode {
    pub fn new(access: InnerClassAccess, inner_name: Rc<str>, name: Option<Rc<str>>, outer_name: Option<Rc<str>>) -> InnerClassNode {
        InnerClassNode {
            access, inner_name, name, outer_name
        }
    }
    pub fn accept(&self, vis: &mut dyn ClassVisitor) {
        vis.visit_inner_class(self.inner_name.clone(), self.outer_name.clone(), self.name.clone(), self.access);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationValue {
    Primitive(AnnotationPrimitive),
    Enum{desc: Rc<str>, value: Rc<str>},
    Nested(AnnotationNode),
    Array(Vec<AnnotationValue>)
}

impl AnnotationValue {
    fn accept(&self, vis: &mut dyn AnnotationVisitor, k: Rc<str>) {
        match self {
            AnnotationValue::Primitive(pr) => {
                vis.visit_primitive(k, pr.clone());
            },
            AnnotationValue::Enum{desc, value} => {
                vis.visit_enum(k, desc.clone(), value.clone());
            },
            AnnotationValue::Array(arr) => {
                if let Some(vis2) = vis.visit_array(k) {
                    for val in arr {
                        val.accept(vis2, Rc::from(""));
                    }
                    vis2.visit_end();
                }
            },
            AnnotationValue::Nested(n) => {
                if let Some(vis2) = vis.visit_annotation(k, n.desc.clone()) {
                    n.accept(vis2);
                }
            }
        }
    }
}

impl AnnotationVisitor for AnnotationValue {
    fn visit_primitive(&mut self, _: Rc<str>, value: AnnotationPrimitive) {
        if let AnnotationValue::Array(values) = self {
            values.push(AnnotationValue::Primitive(value));
        } else {
            panic!();
        }
    }
    fn visit_enum(&mut self, _: Rc<str>, desc: Rc<str>, value: Rc<str>) {
        if let AnnotationValue::Array(values) = self {
            let val = AnnotationValue::Enum {
                desc, value
            };
            values.push(val);
        } else {
            panic!();
        }
    }
    fn visit_annotation(&mut self, _: Rc<str>, desc: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        if let AnnotationValue::Array(values) = self {
            let val = AnnotationValue::Nested(AnnotationNode::new(desc));
            values.push(val);
            if let Some(AnnotationValue::Nested(ref mut val)) = values.last_mut() {
                Some(val)
            } else {
                unreachable!();
            }
        } else {
            panic!();
        }
    }
    fn visit_array(&mut self, _: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        if let AnnotationValue::Array(values) = self {
            let val = AnnotationValue::Array(Vec::new());
            values.push(val);
            values.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            panic!();
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotationNode {
    pub desc: Rc<str>,
    pub values: HashMap<Rc<str>, AnnotationValue>
}

impl AnnotationNode {
    pub fn new(desc: Rc<str>) -> AnnotationNode {
        AnnotationNode {
            desc,
            values: HashMap::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn AnnotationVisitor) {
        for (k, v) in &self.values {
            v.accept(vis, k.clone());
        }
        vis.visit_end();
    }
}
impl AnnotationVisitor for AnnotationNode {
    fn visit_primitive(&mut self, name: Rc<str>, value: AnnotationPrimitive) {
        self.values.insert(name, AnnotationValue::Primitive(value));
    }
    fn visit_enum(&mut self, name: Rc<str>, desc: Rc<str>, value: Rc<str>) {
        let val = AnnotationValue::Enum {
            desc, value
        };
        self.values.insert(name, val);
    }
    fn visit_annotation(&mut self, name: Rc<str>, desc: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        let val = AnnotationValue::Nested(AnnotationNode::new(desc));
        if let AnnotationValue::Nested(ref mut val) = self.values.entry(name).or_insert(val) {
            Some(val)
        } else {
            unreachable!();
        }
    }
    fn visit_array(&mut self, name: Rc<str>) -> Option<&mut dyn AnnotationVisitor> {
        let val = AnnotationValue::Array(Vec::new());
        Some(self.values.entry(name).or_insert(val))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotationNode {
    pub type_ref: TypeRef,
    pub type_path: TypePath,
    pub annotation: AnnotationNode,
}

impl TypeAnnotationNode {
    pub fn new(type_ref: TypeRef, type_path: TypePath, desc: Rc<str>) -> TypeAnnotationNode {
        let annotation = AnnotationNode::new(desc);
        TypeAnnotationNode {
            type_path, type_ref, annotation
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalVariableAnnotationNode {
    data: TypeAnnotationNode,
    spans: Vec<LocalVariableSpan>
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionNode {
    pub data: InstructionData,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionData {
    NoArgInsn{opcode: u8},
    FieldMethodInsn{opcode: u8, owner: Rc<str>, method: NameAndType},
    IincInsn{var: u16, by: i16},
    IntInsn{opcode: u8, operand: i32},
    InvokeDynamic{bsm: Handle, args: Vec<ClassConstant>, method: NameAndType},
    JumpInsn{opcode: u8, label: Label},
    LabelNode{label: Label},
    LdcInsn{data: ClassConstant},
    LineNumberNode{line: u16, start: Label},
    LookupswitchInsn{dflt: Label, pairs: Vec<(i32, Label)>},
    MultianewInsn{ty: Rc<str>, dims: u8},
    TableswitchInsn{min: i32, max: i32, dflt: Label, labels: Vec<Label>},
    TypeInsn{opcode: u8, ty: Rc<str>},
    VarInsn{opcode: u8, var: u16},
    FrameNode{mode: FrameMode, locals: Vec<FrameItem>, stack: Vec<FrameItem>}
}

impl InstructionNode {
    pub fn new_raw(data: InstructionData) -> InstructionNode {
        InstructionNode {
            data,
            invisible_type_annotations: Vec::new(),
            visible_type_annotations: Vec::new()
        }
    }
    pub fn new_insn(opcode: u8) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::NoArgInsn {
            opcode
        })
    }
    pub fn new_field_method_insn(opcode: u8, owner: Rc<str>, method: NameAndType) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::FieldMethodInsn {
            opcode, owner, method
        })
    }
    pub fn new_iinc_insn(var: u16, by: i16) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::IincInsn {
            var, by
        })
    }
    pub fn new_int_insn(opcode: u8, operand: i32) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::IntInsn {
            opcode, operand
        })
    }
    pub fn new_indy_insn(bsm: Handle, args: Vec<ClassConstant>, method: NameAndType) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::InvokeDynamic {
            bsm, args, method
        })
    }
    pub fn new_jump_insn(opcode: u8, label: Label) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::JumpInsn {
            opcode, label
        })
    }
    pub fn new_label_insn(label: Label) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::LabelNode {
            label
        })
    }
    pub fn new_ldc_insn(data: ClassConstant) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::LdcInsn {
            data
        })
    }
    pub fn new_line_number_insn(line: u16, start: Label) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::LineNumberNode {
            line, start
        })
    }
    pub fn new_lookupswitch_insn(dflt: Label, pairs: Vec<(i32, Label)>) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::LookupswitchInsn {
            dflt, pairs
        })
    }
    pub fn new_multianew_insn(ty: Rc<str>, dims: u8) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::MultianewInsn {
            ty, dims
        })
    }
    pub fn new_tableswitch_insn(min: i32, max: i32, dflt: Label, labels: Vec<Label>) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::TableswitchInsn {
            min, max, dflt, labels
        })
    }
    pub fn new_type_insn(opcode: u8, ty: Rc<str>) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::TypeInsn {
            opcode, ty
        })
    }
    pub fn new_var_insn(opcode: u8, var: u16) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::VarInsn {
            opcode, var
        })
    }
    pub fn new_frame_insn(mode: FrameMode, locals: Vec<FrameItem>, stack: Vec<FrameItem>) -> InstructionNode {
        InstructionNode::new_raw(InstructionData::FrameNode {
            mode, locals, stack
        })
    }
    pub fn accept(&self, vis: &mut dyn MethodVisitor) {
        match &self.data {
            InstructionData::NoArgInsn {opcode} => {
                vis.visit_insn(*opcode);
            },
            InstructionData::FieldMethodInsn {opcode, owner, method} => {
                vis.visit_field_method_insn(*opcode, owner.clone(), method.clone());
            },
            InstructionData::IincInsn {var, by} => {
                vis.visit_iinc_insn(*var, *by);
            },
            InstructionData::IntInsn {opcode, operand} => {
                vis.visit_int_insn(*opcode, *operand);
            },
            InstructionData::InvokeDynamic {bsm, args, method} => {
                vis.visit_indy_insn(method.clone(), bsm.clone(), args.clone());
            },
            InstructionData::JumpInsn {opcode, label} => {
                vis.visit_jump_insn(*opcode, *label);
            },
            InstructionData::LabelNode {label} => {
                vis.visit_label(*label);
            },
            InstructionData::LdcInsn {data} => {
                vis.visit_ldc_insn(data.clone());
            },
            InstructionData::LineNumberNode {line, start} => {
                vis.visit_line_number(*line, *start);
            },
            InstructionData::LookupswitchInsn {dflt, pairs} => {
                vis.visit_lookupswitch_insn(*dflt, pairs.clone());
            },
            InstructionData::MultianewInsn{ty, dims} => {
                vis.visit_multianew_insn(ty.clone(), *dims);
            },
            InstructionData::TableswitchInsn{min, max, dflt, labels} => {
                vis.visit_tableswitch_insn(*min, *max, *dflt, labels.clone());
            },
            InstructionData::TypeInsn{opcode, ty} => {
                vis.visit_type_insn(*opcode, ty.clone());
            },
            InstructionData::VarInsn{opcode, var} => {
                vis.visit_var_insn(*opcode, *var);
            },
            InstructionData::FrameNode {mode, locals, stack} => {
                vis.visit_frame(*mode, locals, stack);
            }
        }
        for tann in &self.visible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), true) {
                tann.annotation.accept(av);
            }
        }
        for tann in &self.invisible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), false) {
                tann.annotation.accept(av);
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ParameterNode {
    pub access: ParameterAccess,
    pub name: Rc<str>,
}

impl ParameterNode {
    pub fn new(access: ParameterAccess, name: Rc<str>) -> ParameterNode {
        ParameterNode {
            access, name
        }
    }
    pub fn accept(&self, vis: &mut dyn MethodVisitor) {
        vis.visit_parameter(self.name.clone(), self.access);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalVariableNode {
    pub id: NameAndType,
    pub span: LocalVariableSpan,
    pub signature: Option<Rc<str>>,
}

impl LocalVariableNode {
    pub fn new(id: NameAndType, signature: Option<Rc<str>>, start: Label, index: u16, end: Label) -> LocalVariableNode {
        let span = LocalVariableSpan {
            start, index, end
        };
        LocalVariableNode {
            id, signature, span
        }
    }
    pub fn accept(&self, vis: &mut dyn MethodVisitor) {
        vis.visit_local_var(self.id.clone(), self.signature.clone(), self.span);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryCatchBlockNode {
    pub start: Label,
    pub handler: Label,
    pub end: Label,
    pub catch_type: Option<Rc<str>>,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>
}

impl TryCatchBlockNode {
    pub fn new(start: Label, end: Label, handler: Label, catch_type: Option<Rc<str>>) -> TryCatchBlockNode {
        TryCatchBlockNode {
            start, end, handler, catch_type,
            invisible_type_annotations: Vec::new(),
            visible_type_annotations: Vec::new(),
        }
    }
    pub fn accept(&self, vis: &mut dyn MethodVisitor) {
        vis.visit_try_catch(self.start, self.end, self.handler, self.catch_type.clone());
        for tann in &self.visible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), true) {
                tann.annotation.accept(av);
            }
        }
        for tann in &self.invisible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), false) {
                tann.annotation.accept(av);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodNode {
    pub access: MethodAccess,
    pub annotation_default: Option<AnnotationNode>,
    pub id: NameAndType,
    pub exceptions: Vec<Rc<str>>,
    pub instructions: Vec<InstructionNode>,
    pub invisible_annotable_parameter_count: Option<u8>,
    pub invisible_annotations: Vec<AnnotationNode>,
    pub invisible_local_variable_annotations: Vec<LocalVariableAnnotationNode>,
    pub invisible_parameter_annotations: Vec<Vec<AnnotationNode>>,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub local_variables: Vec<LocalVariableNode>,
    pub max_locals: u16,
    pub max_stack: u16,
    pub parameters: Vec<ParameterNode>,
    pub signature: Option<Rc<str>>,
    pub try_catch_blocks: Vec<TryCatchBlockNode>,
    pub visible_annotable_parameter_count: Option<u8>,
    pub visible_annotations: Vec<AnnotationNode>,
    pub visible_local_variable_annotations: Vec<LocalVariableAnnotationNode>,
    pub visible_parameter_annotations: Vec<Vec<AnnotationNode>>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>,
}

impl MethodNode {
    pub fn new(access: MethodAccess, id: NameAndType, signature: Option<Rc<str>>, exceptions: Vec<Rc<str>>) -> MethodNode {
        MethodNode {
            access, id, signature, exceptions,
            annotation_default: None,
            instructions: Vec::new(),
            invisible_annotable_parameter_count: None,
            invisible_annotations: Vec::new(),
            invisible_local_variable_annotations: Vec::new(),
            invisible_parameter_annotations: Vec::new(),
            invisible_type_annotations: Vec::new(),
            local_variables: Vec::new(),
            max_locals: 0,
            max_stack: 0,
            parameters: Vec::new(),
            try_catch_blocks: Vec::new(),
            visible_annotable_parameter_count: None,
            visible_annotations: Vec::new(),
            visible_local_variable_annotations: Vec::new(),
            visible_parameter_annotations: Vec::new(),
            visible_type_annotations: Vec::new(),
        }
    }
    pub fn new_empty() -> MethodNode {
        MethodNode::new(MethodAccess::empty(), NameAndType::new(Rc::from(""), Rc::from("")), None, Vec::new())
    }
    pub fn accept_cv(&self, vis: &mut dyn ClassVisitor) {
        if let Some(mv) = vis.visit_method(self.access, self.id.clone(), self.signature.clone(), self.exceptions.clone()) {
            self.accept_mv(mv);
        }
    }
    pub fn accept_mv(&self, vis: &mut dyn MethodVisitor) {
        for par in &self.parameters {
            par.accept(vis);
        }
        if let Some(vl) = &self.annotation_default {
            if let Some(av) = vis.visit_annotation_default() {
                vl.accept(av);
            }
        }
        for ann in &self.visible_annotations {
            if let Some(av) = vis.visit_annotation(ann.desc.clone(), true) {
                ann.accept(av);
            }
        }
        for ann in &self.invisible_annotations {
            if let Some(av) = vis.visit_annotation(ann.desc.clone(), false) {
                ann.accept(av);
            }
        }
        if let Some(num) = self.visible_annotable_parameter_count {
            vis.visit_annotable_parameter_count(num, true);
        }
        if let Some(num) = self.invisible_annotable_parameter_count {
            vis.visit_annotable_parameter_count(num, false);
        }
        for i in 0..self.visible_parameter_annotations.len() {
            let annset = &self.visible_parameter_annotations[i];
            for ann in annset {
                if let Some(av) = vis.visit_parameter_annotation(i as u8, ann.desc.clone(), false) {
                    ann.accept(av);
                }
            }
        }
        for i in 0..self.invisible_parameter_annotations.len() {
            let annset = &self.invisible_parameter_annotations[i];
            for ann in annset {
                if let Some(av) = vis.visit_parameter_annotation(i as u8, ann.desc.clone(), false) {
                    ann.accept(av);
                }
            }
        }
        for tann in &self.visible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), true) {
                tann.annotation.accept(av);
            }
        }
        for tann in &self.invisible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), false) {
                tann.annotation.accept(av);
            }
        }
        vis.visit_code();
        for tcb in &self.try_catch_blocks {
            tcb.accept(vis);
        }
        for insn in &self.instructions {
            insn.accept(vis);
        }
        for locv in &self.local_variables {
            locv.accept(vis);
        }
        vis.visit_maxs(self.max_stack, self.max_locals);
        vis.visit_end();
    }
}
impl MethodVisitor for MethodNode {
    fn visit_parameter(&mut self, name: Rc<str>, access: ParameterAccess) {
        let par = ParameterNode::new(access, name);
        self.parameters.push(par);
    }
    fn visit_annotation_default(&mut self) -> Option<&mut dyn AnnotationVisitor> {
        self.annotation_default = Some(AnnotationNode::new(Rc::from("")));
        self.annotation_default.as_mut().map(|c| c as &mut dyn AnnotationVisitor)
    }
    fn visit_field_method_insn(&mut self, opcode: u8, owner: Rc<str>, method: NameAndType) {
        let is = InstructionNode::new_field_method_insn(opcode, owner, method);
        self.instructions.push(is);
    }
    fn visit_insn(&mut self, opcode: u8) {
        let is = InstructionNode::new_insn(opcode);
        self.instructions.push(is);
    }
    fn visit_iinc_insn(&mut self, var: u16, by: i16) {
        let is = InstructionNode::new_iinc_insn(var, by);
        self.instructions.push(is);
    }
    fn visit_int_insn(&mut self, opcode: u8, operand: i32) {
        let is = InstructionNode::new_int_insn(opcode, operand);
        self.instructions.push(is);
    }
    fn visit_indy_insn(&mut self, method: NameAndType, bsm: Handle, args: Vec<ClassConstant>) {
        let is = InstructionNode::new_indy_insn(bsm, args, method);
        self.instructions.push(is);
    }
    fn visit_jump_insn(&mut self, opcode: u8, label: Label) {
        let is = InstructionNode::new_jump_insn(opcode, label);
        self.instructions.push(is);
    }
    fn visit_label(&mut self, label: Label) {
        let is = InstructionNode::new_label_insn(label);
        self.instructions.push(is);
    }
    fn visit_ldc_insn(&mut self, cst: ClassConstant) {
        let is = InstructionNode::new_ldc_insn(cst);
        self.instructions.push(is);
    }
    fn visit_line_number(&mut self, line: u16, label: Label) {
        let is = InstructionNode::new_line_number_insn(line, label);
        self.instructions.push(is);
    }
    fn visit_lookupswitch_insn(&mut self, dflt: Label, pairs: Vec<(i32, Label)>) {
        let is = InstructionNode::new_lookupswitch_insn(dflt, pairs);
        self.instructions.push(is);
    }
    fn visit_multianew_insn(&mut self, class: Rc<str>, dimensions: u8) {
        let is = InstructionNode::new_multianew_insn(class, dimensions);
        self.instructions.push(is);
    }
    fn visit_tableswitch_insn(&mut self, min: i32, max: i32, dflt: Label, labels: Vec<Label>) {
        let is = InstructionNode::new_tableswitch_insn(min, max, dflt, labels);
        self.instructions.push(is);
    }
    fn visit_type_insn(&mut self, opcode: u8, class: Rc<str>) {
        let is = InstructionNode::new_type_insn(opcode, class);
        self.instructions.push(is);
    }
    fn visit_var_insn(&mut self, opcode: u8, var: u16) {
        let is = InstructionNode::new_var_insn(opcode, var);
        self.instructions.push(is);
    }
    fn visit_frame(&mut self, mode: FrameMode, locals: &[FrameItem], stack: &[FrameItem]) {
        let is = InstructionNode::new_frame_insn(mode, locals.into(), stack.into());
        self.instructions.push(is);
    }

    fn visit_annotable_parameter_count(&mut self, count: u8, visible: bool) {
        if visible {
            self.visible_annotable_parameter_count = Some(count);
        } else {
            self.invisible_annotable_parameter_count = Some(count);
        }
    }
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = AnnotationNode::new(desc);
        if visible {
            self.visible_annotations.push(at);
            self.visible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_annotations.push(at);
            self.invisible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.visible_type_annotations.push(at);
            self.visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_type_annotations.push(at);
            self.invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_trycatch_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.try_catch_blocks.last_mut().unwrap().visible_type_annotations.push(at);
            self.try_catch_blocks.last_mut().unwrap().visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.try_catch_blocks.last_mut().unwrap().invisible_type_annotations.push(at);
            self.try_catch_blocks.last_mut().unwrap().invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_insn_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.instructions.last_mut().unwrap().visible_type_annotations.push(at);
            self.instructions.last_mut().unwrap().visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.instructions.last_mut().unwrap().invisible_type_annotations.push(at);
            self.instructions.last_mut().unwrap().invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_local_variable_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, spans: Vec<LocalVariableSpan>, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let data = TypeAnnotationNode::new(type_ref, type_path, desc);
        let at = LocalVariableAnnotationNode {data, spans};
        if visible {
            self.invisible_local_variable_annotations.push(at);
            self.invisible_local_variable_annotations.last_mut().map(|c| &mut c.data.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.visible_local_variable_annotations.push(at);
            self.visible_local_variable_annotations.last_mut().map(|c| &mut c.data.annotation as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_parameter_annotation(&mut self, parameter: u8, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let parameter = parameter as usize;
        let data = AnnotationNode::new(desc);
        if visible {
            let nl = max(self.visible_parameter_annotations.len(), parameter + 1);
            self.visible_parameter_annotations.resize(nl, Vec::new());
            self.visible_parameter_annotations[parameter].push(data);
            self.visible_parameter_annotations[parameter].last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            let nl = max(self.visible_parameter_annotations.len(), parameter + 1);
            self.invisible_parameter_annotations.resize(nl, Vec::new());
            self.invisible_parameter_annotations[parameter].push(data);
            self.invisible_parameter_annotations[parameter].last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        }
    }
    fn visit_maxs(&mut self, max_stack: u16, max_local: u16) {
        self.max_locals = max_local;
        self.max_stack = max_stack;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExportNode {
    pub access: ExportFlags,
    pub modules: Vec<Rc<str>>,
    pub package: Rc<str>
}

impl ModuleExportNode {
    pub fn new(package: Rc<str>, access: ExportFlags, modules: Vec<Rc<str>>) -> ModuleExportNode {
        ModuleExportNode {
            access, modules, package
        }
    }
    pub fn accept(&self, vis: &mut dyn ModuleVisitor) {
        vis.visit_export(self.package.clone(), self.access, self.modules.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleOpenNode {
    pub access: ExportFlags,
    pub modules: Vec<Rc<str>>,
    pub package: Rc<str>
}

impl ModuleOpenNode {
    pub fn new(package: Rc<str>, access: ExportFlags, modules: Vec<Rc<str>>) -> ModuleOpenNode {
        ModuleOpenNode {
            access, modules, package
        }
    }
    pub fn accept(&self, vis: &mut dyn ModuleVisitor) {
        vis.visit_open(self.package.clone(), self.access, self.modules.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleProvideNode {
    pub providers: Vec<Rc<str>>,
    pub service: Rc<str>
}

impl ModuleProvideNode {
    pub fn new(service: Rc<str>, providers: Vec<Rc<str>>) -> ModuleProvideNode {
        ModuleProvideNode {
            service, providers
        }
    }
    pub fn accept(&self, vis: &mut dyn ModuleVisitor) {
        vis.visit_provide(self.service.clone(), self.providers.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleRequireNode {
    pub access: RequireFlags,
    pub module: Rc<str>,
    pub version: Option<Rc<str>>
}

impl ModuleRequireNode {
    pub fn new(module: Rc<str>, access: RequireFlags, version: Option<Rc<str>>) -> ModuleRequireNode {
        ModuleRequireNode {
            module, access, version
        }
    }
    pub fn accept(&self, vis: &mut dyn ModuleVisitor) {
        vis.visit_require(self.module.clone(), self.access, self.version.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleNode {
    pub access: ModuleFlags,
    pub exports: Vec<ModuleExportNode>,
    pub main_class: Option<Rc<str>>,
    pub name: Rc<str>,
    pub opens: Vec<ModuleOpenNode>,
    pub packages: Vec<Rc<str>>,
    pub provides: Vec<ModuleProvideNode>,
    pub requires: Vec<ModuleRequireNode>,
    pub uses: Vec<Rc<str>>,
    pub version: Option<Rc<str>>
}

impl ModuleNode {
    pub fn new(name: Rc<str>, access: ModuleFlags, version: Option<Rc<str>>) -> ModuleNode {
        ModuleNode {
            name, access, version,
            exports: Vec::new(),
            main_class: None,
            opens: Vec::new(),
            packages: Vec::new(),
            provides: Vec::new(),
            requires: Vec::new(),
            uses: Vec::new(),
        }
    }
    pub fn accept(&self, vis: &mut dyn ModuleVisitor) {
        if let Some(ref c) = self.main_class {
            vis.visit_main_class(c.clone());
        }
        for package in &self.packages {
            vis.visit_package(package.clone());
        }
        for require in &self.requires {
            require.accept(vis);
        }
        for export in &self.exports {
            export.accept(vis);
        }
        for open in &self.opens {
            open.accept(vis);
        }
        for uze in &self.uses {
            vis.visit_use(uze.clone());
        }
        for provide in &self.provides {
            provide.accept(vis);
        }
        vis.visit_end();
    }
}
impl ModuleVisitor for ModuleNode {
    fn visit_main_class(&mut self, name: Rc<str>) {
        self.main_class = Some(name);
    }
    fn visit_package(&mut self, pkg: Rc<str>) {
        self.packages.push(pkg);
    }
    fn visit_require(&mut self, module: Rc<str>, flags: RequireFlags, version: Option<Rc<str>>) {
        let rq = ModuleRequireNode::new(module, flags, version);
        self.requires.push(rq);
    }
    fn visit_export(&mut self, pkg: Rc<str>, flags: ExportFlags, modules: Vec<Rc<str>>) {
        let ex = ModuleExportNode::new(pkg, flags, modules);
        self.exports.push(ex);
    }
    fn visit_open(&mut self, pkg: Rc<str>, flags: ExportFlags, modules: Vec<Rc<str>>) {
        let op = ModuleOpenNode::new(pkg, flags, modules);
        self.opens.push(op);
    }
    fn visit_use(&mut self, service: Rc<str>) {
        self.uses.push(service);
    }
    fn visit_provide(&mut self, service: Rc<str>, providers: Vec<Rc<str>>) {
        let pr = ModuleProvideNode::new(service, providers);
        self.provides.push(pr);
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassNode {
    pub access: ClassAccess,
    pub fields: Vec<FieldNode>,
    pub record_components: Vec<RecordComponentNode>,
    pub inner_classes: Vec<InnerClassNode>,
    pub interfaces: Vec<Rc<str>>,
    pub invisible_annotations: Vec<AnnotationNode>,
    pub invisible_type_annotations: Vec<TypeAnnotationNode>,
    pub methods: Vec<MethodNode>,
    pub module: Option<ModuleNode>,
    pub name: Rc<str>,
    pub nest_host_class: Option<Rc<str>>,
    pub nest_members: Vec<Rc<str>>,
    pub outer_class: Option<Rc<str>>,
    pub outer_method: Option<NameAndType>,
    pub signature: Option<Rc<str>>,
    pub source_debug: Option<String>,
    pub source_name: Option<Rc<str>>,
    pub super_name: Option<Rc<str>>,
    pub version: ClassVersion,
    pub visible_annotations: Vec<AnnotationNode>,
    pub visible_type_annotations: Vec<TypeAnnotationNode>
}

impl ClassVisitor for ClassNode {
    fn visit_header(&mut self, version: ClassVersion, access: ClassAccess, name: Rc<str>,
            signature: Option<Rc<str>>, super_name: Option<Rc<str>>, interfaces: Vec<Rc<str>>) {
        self.version = version;
        self.access = access;
        self.signature = signature;
        self.super_name = super_name;
        self.name = name;
        self.interfaces = interfaces;
    }
    fn visit_nest_host(&mut self, name: Rc<str>) {
        self.nest_host_class = Some(name);
    }
    fn visit_nest_member(&mut self, name: Rc<str>) {
        self.nest_members.push(name);
    }
    fn visit_source(&mut self, source: Option<Rc<str>>, debug: Option<String>) {
        self.source_name = source;
        self.source_debug = debug;
    }
    fn visit_outer_class(&mut self, owner: Rc<str>, method: Option<NameAndType>) {
        self.outer_class = Some(owner);
        self.outer_method = method;
    }
    fn visit_inner_class(&mut self, inner_name: Rc<str>, outer_name: Option<Rc<str>>, simple_inner_name: Option<Rc<str>>, access: InnerClassAccess) {
        let nd = InnerClassNode::new(access, inner_name, simple_inner_name, outer_name);
        self.inner_classes.push(nd);
    }
    fn visit_module(&mut self, name: Rc<str>, flags: ModuleFlags, version: Option<Rc<str>>) -> Option<&mut dyn ModuleVisitor> {
        let md = ModuleNode::new(name, flags, version);
        self.module = Some(md);
        self.module.as_mut().map(|c| c as &mut dyn ModuleVisitor)
    }
    fn visit_record_component(&mut self, record_component: NameAndType, signature: Option<Rc<str>>) -> Option<&mut dyn RecordComponentVisitor> {
        let record_comp = RecordComponentNode::new(record_component, signature);
        self.record_components.push(record_comp);
        self.record_components.last_mut().map(|c| c as &mut dyn RecordComponentVisitor)
    }
    fn visit_field(&mut self, access: FieldAccess, field: NameAndType, signature: Option<Rc<str>>, value: Option<ClassConstant>) -> Option<&mut dyn FieldVisitor> {
        let field = FieldNode::new(access, field, signature, value);
        self.fields.push(field);
        self.fields.last_mut().map(|c| c as &mut dyn FieldVisitor)
    }
    fn visit_method(&mut self, access: MethodAccess, method: NameAndType, signature: Option<Rc<str>>, exceptions: Vec<Rc<str>>) -> Option<&mut dyn MethodVisitor> {
        let mt = MethodNode::new(access, method, signature, exceptions);
        self.methods.push(mt);
        self.methods.last_mut().map(|c| c as &mut dyn MethodVisitor)
    }
    fn visit_annotation(&mut self, desc: Rc<str>, visible: bool) -> Option<&mut dyn AnnotationVisitor> {
        let at = AnnotationNode::new(desc);
        if visible {
            self.visible_annotations.push(at);
            self.visible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_annotations.push(at);
            self.invisible_annotations.last_mut().map(|c| c as &mut dyn AnnotationVisitor)
        }

    }
    fn visit_type_annotation(&mut self, type_ref: TypeRef, type_path: TypePath, desc: Rc<str>, visible: bool)-> Option<&mut dyn AnnotationVisitor> {
        let at = TypeAnnotationNode::new(type_ref, type_path, desc);
        if visible {
            self.visible_type_annotations.push(at);
            self.visible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        } else {
            self.invisible_type_annotations.push(at);
            self.invisible_type_annotations.last_mut().map(|c| &mut c.annotation as &mut dyn AnnotationVisitor)
        }
    }
}

impl Default for ClassNode {
    fn default() -> Self {
        Self::new()
    }
}

impl ClassNode {
    pub fn new() -> ClassNode {
        ClassNode {
            access: ClassAccess::empty(),
            fields: Vec::new(),
            inner_classes: Vec::new(),
            interfaces: Vec::new(),
            invisible_annotations: Vec::new(),
            invisible_type_annotations: Vec::new(),
            methods: Vec::new(),
            module: None,
            name: Rc::from(""),
            nest_host_class: None,
            nest_members: Vec::new(),
            outer_class: None,
            outer_method: None,
            signature: None,
            source_debug: None,
            source_name: None,
            super_name: None,
            version: ClassVersion::new(0, 0),
            visible_annotations: Vec::new(),
            visible_type_annotations: Vec::new(),
            record_components: Vec::new()
        }
    }
    pub fn from_bytes(bytes: &[u8]) -> ClassNode {
        let reader = crate::reader::ClassReader::new(&bytes);
        let mut visitor = ClassNode::new();
        reader.accept(&mut visitor, crate::reader::ClassReaderFlags::empty()).unwrap();
        visitor
    }
    pub fn accept(&self, vis: &mut dyn ClassVisitor) {
        vis.visit_header(self.version, self.access, self.name.clone(), self.signature.clone(), self.super_name.clone(), self.interfaces.clone());
        if self.source_name.is_some() || self.source_debug.is_some() {
            vis.visit_source(self.source_name.clone(), self.source_debug.clone());
        }
        if let Some(md) = &self.module {
            if let Some(mv) = vis.visit_module(md.name.clone(), md.access, md.version.clone()) {
                md.accept(mv);
            }
        }
        if let Some(nh) = &self.nest_host_class {
            vis.visit_nest_host(nh.clone());
        }
        if let Some(oc) = &self.outer_class {
            vis.visit_outer_class(oc.clone(), self.outer_method.clone());
        }
        for ann in &self.visible_annotations {
            if let Some(av) = vis.visit_annotation(ann.desc.clone(), true) {
                ann.accept(av);
            }
        }
        for ann in &self.invisible_annotations {
            if let Some(av) = vis.visit_annotation(ann.desc.clone(), false) {
                ann.accept(av);
            }
        }
        for tann in &self.visible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), true) {
                tann.annotation.accept(av);
            }
        }
        for tann in &self.invisible_type_annotations {
            if let Some(av) = vis.visit_type_annotation(tann.type_ref, tann.type_path.clone(), tann.annotation.desc.clone(), false) {
                tann.annotation.accept(av);
            }
        }
        for nm in &self.nest_members {
            vis.visit_nest_member(nm.clone());
        }
        for ic in &self.inner_classes {
            ic.accept(vis);
        }
        for fi in &self.fields {
            fi.accept(vis);
        }
        for rc in &self.record_components {
            rc.accept(vis);
        }
        for me in &self.methods {
            me.accept_cv(vis);
        }
        vis.visit_end();
    }
}