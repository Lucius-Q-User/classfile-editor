use super::{TypeConstraintKind, SignatureVisitor};


#[derive(Debug, PartialEq, Clone)]
pub struct TypeParameterNode<'a> {
    pub identifier: &'a str,
    pub superclass_bound: Option<ReferenceTypeNode<'a>>,
    pub interface_bound: Vec<ReferenceTypeNode<'a>>
}

impl<'a> TypeParameterNode<'a> {
    pub fn new(identifier: &str) -> TypeParameterNode {
        TypeParameterNode {
            identifier,
            superclass_bound: None,
            interface_bound: Vec::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn SignatureVisitor<'a>) {
        vis.visit_formal_type_parameter(self.identifier);   
        if let Some(rtn) = &self.superclass_bound {
            if let Some(vs) = vis.visit_class_bound() {
                rtn.accept(vs);
            }
        }
        for itf in &self.interface_bound {
            if let Some(vs) = vis.visit_interface_bound() {
                itf.accept(vs);
            }
        }
    }
}


#[derive(Debug, PartialEq, Clone, Default)]
pub struct ReferenceTypeNode<'a> {
    pub data: ReferenceTypeData<'a>,
    pub array_dimensions: u8
}

#[derive(Debug, PartialEq, Clone)]
pub enum ReferenceTypeData<'a> {
    TypeVariable(&'a str),
    ClassType(Vec<SimpleClassTypeSignature<'a>>),
    BaseType(char)
}
impl<'a> Default for ReferenceTypeData<'a> {
    fn default() -> Self {
        ReferenceTypeData::BaseType('\0')
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SimpleClassTypeSignature<'a> {
    pub name: &'a str,
    pub type_arguments: Vec<TypeArgument<'a>>
}

impl<'a> SignatureVisitor<'a> for ReferenceTypeNode<'a> {
    fn visit_base_type(&mut self, ty: char) {
        self.data = ReferenceTypeData::BaseType(ty);
    }
    fn visit_type_variable(&mut self, name: &'a str) {
        self.data = ReferenceTypeData::TypeVariable(name);
    }
    fn visit_array_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.array_dimensions += 1;
        Some(self)
    }   
    fn visit_class_type(&mut self, name: &'a str) {
        let mut cts = Vec::new();
        let st = SimpleClassTypeSignature {
            type_arguments: Vec::new(),
            name
        };
        cts.push(st);
        self.data = ReferenceTypeData::ClassType(cts);
    }
    fn visit_inner_class_type(&mut self, name: &'a str) {
        if let ReferenceTypeData::ClassType(cts) = &mut self.data {
            let st = SimpleClassTypeSignature {
                type_arguments: Vec::new(),
                name
            };
            cts.push(st);
        } else {
            panic!();
        }
    }
    fn visit_unbound_type_argument(&mut self) {
        if let ReferenceTypeData::ClassType(cts) = &mut self.data {
            let arg = TypeArgument {
                constraint: TypeConstraintKind::Unbound,
                value: None    
            };
            cts.last_mut().unwrap().type_arguments.push(arg);
        } else {
            panic!();
        }
    }
    fn visit_type_argument<'b>(&'b mut self, arg: TypeConstraintKind) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let ReferenceTypeData::ClassType(cts) = &mut self.data {
            let arg = TypeArgument {
                constraint: arg,
                value: Some(ReferenceTypeNode::new_base('\0'))    
            };
            cts.last_mut().unwrap().type_arguments.push(arg);
            cts.last_mut().unwrap().type_arguments.last_mut().map(|c| c.value.as_mut().unwrap() as &mut dyn SignatureVisitor)
        } else {
            panic!();
        }
    }
}

impl<'a> ReferenceTypeNode<'a> {
    pub fn accept(&self, mut vis: &mut dyn SignatureVisitor<'a>) {
        for _ in 0..self.array_dimensions {
            let vs = vis.visit_array_type();
            if vs.is_some() {
                vis = vs.unwrap()
            } else {
                return;
            }
        }
        match &self.data {
            ReferenceTypeData::TypeVariable(name) => {
                vis.visit_type_variable(name);
            },
            ReferenceTypeData::BaseType(ty) => {
                vis.visit_base_type(*ty);
            },
            ReferenceTypeData::ClassType(ctd) => {
                vis.visit_class_type(ctd[0].name);
                for at in &ctd[0].type_arguments {
                    if at.constraint == TypeConstraintKind::Unbound {
                        vis.visit_unbound_type_argument();
                    } else if let Some(vs) = vis.visit_type_argument(at.constraint) {
                        at.value.as_ref().unwrap().accept(vs);
                    }   
                }
                for ic in &ctd[1..] {
                    vis.visit_inner_class_type(ic.name);
                    for at in &ic.type_arguments {
                        if at.constraint == TypeConstraintKind::Unbound {
                            vis.visit_unbound_type_argument();
                        } else if let Some(vs) = vis.visit_type_argument(at.constraint) {
                            at.value.as_ref().unwrap().accept(vs);
                        }   
                    }
                }
                vis.visit_end();
            }
        }

    }
    pub fn new_base<'b>(ty: char) -> ReferenceTypeNode<'b> {
        let data = ReferenceTypeData::BaseType(ty);
        ReferenceTypeNode {
            data,
            array_dimensions: 0
        }
    }
    pub fn new_type_var(ty: &str) -> ReferenceTypeNode {
        let data = ReferenceTypeData::TypeVariable(ty);
        ReferenceTypeNode {
            data,
            array_dimensions: 0
        }
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeArgument<'a> {
    pub constraint: TypeConstraintKind,
    pub value: Option<ReferenceTypeNode<'a>>
}
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ClassSignature<'a> {
    pub type_parameters: Vec<TypeParameterNode<'a>>,
    pub superclass_bound: ReferenceTypeNode<'a>,
    pub interface_bound: Vec<ReferenceTypeNode<'a>>
}

impl<'a> SignatureVisitor<'a> for ClassSignature<'a> {
    fn get_wrapped_visitor<'b>(&'b self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        panic!();
    }
    fn visit_formal_type_parameter(&mut self, name: &'a str) {
        let tn = TypeParameterNode::new(name);
        self.type_parameters.push(tn);
    }
    fn visit_class_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.type_parameters.last_mut().unwrap().superclass_bound = Some(ReferenceTypeNode::new_base('\0'));
        self.type_parameters.last_mut().unwrap().superclass_bound.as_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
    fn visit_interface_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.type_parameters.last_mut().unwrap().interface_bound.push(ReferenceTypeNode::new_base('\0'));
        self.type_parameters.last_mut().unwrap().interface_bound.last_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
    fn visit_superclass<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        Some(&mut self.superclass_bound)
    }
    fn visit_interface<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.interface_bound.push(ReferenceTypeNode::new_base('\0'));
        self.interface_bound.last_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
}
impl<'a> ClassSignature<'a> {
    pub fn new<'b>() -> ClassSignature<'b> {
        ClassSignature {
            type_parameters: Vec::new(),
            superclass_bound: ReferenceTypeNode::new_base('\0'),
            interface_bound: Vec::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn SignatureVisitor<'a>) {
        for par in &self.type_parameters {
            par.accept(vis);
        }
        if let Some(vs) = vis.visit_superclass() {
            self.superclass_bound.accept(vs);
        }
        for itf in &self.interface_bound {
            if let Some(vs) = vis.visit_interface() {
                itf.accept(vs);
            }
        }
    }
}


#[derive(Debug, PartialEq, Clone, Default)]
pub struct MethodSignature<'a> {
    pub type_parameters: Vec<TypeParameterNode<'a>>,
    pub parameter_types: Vec<ReferenceTypeNode<'a>>,
    pub result: ReferenceTypeNode<'a>,
    pub throws: Vec<ReferenceTypeNode<'a>>
}

impl<'a> SignatureVisitor<'a> for MethodSignature<'a> {
    fn get_wrapped_visitor<'b>(&'b self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        panic!();
    }
    fn visit_formal_type_parameter(&mut self, name: &'a str) {
        let tn = TypeParameterNode::new(name);
        self.type_parameters.push(tn);
    }
    fn visit_class_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.type_parameters.last_mut().unwrap().superclass_bound = Some(ReferenceTypeNode::new_base('\0'));
        self.type_parameters.last_mut().unwrap().superclass_bound.as_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
    fn visit_interface_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.type_parameters.last_mut().unwrap().interface_bound.push(ReferenceTypeNode::new_base('\0'));
        self.type_parameters.last_mut().unwrap().interface_bound.last_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
    fn visit_parameter_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.parameter_types.push(ReferenceTypeNode::new_base('\0'));
        self.parameter_types.last_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
    fn visit_return_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        Some(&mut self.result)
    }
    fn visit_exception_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        self.throws.push(ReferenceTypeNode::new_base('\0'));
        self.parameter_types.last_mut().map(|c| c as &mut dyn SignatureVisitor<'a>)
    }
}

impl<'a> MethodSignature<'a> {
    pub fn new<'b>() -> MethodSignature<'b> {
        MethodSignature {
            type_parameters: Vec::new(),
            parameter_types: Vec::new(),
            result: ReferenceTypeNode::new_base('V'),
            throws: Vec::new()
        }
    }
    pub fn accept(&self, vis: &mut dyn SignatureVisitor<'a>) {
        for par in &self.type_parameters {
            par.accept(vis);
        }
        for par in &self.parameter_types {
            if let Some(vs) = vis.visit_parameter_type() {
                par.accept(vs);
            }
        }
        if let Some(vs) = vis.visit_return_type() {
            self.result.accept(vs);
        }
        for xc in &self.throws {
            if let Some(vs) = vis.visit_exception_type() {
                xc.accept(vs);
            }
        }
    }
}