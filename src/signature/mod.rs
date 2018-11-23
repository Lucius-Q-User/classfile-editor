#[cfg(feature = "signature_tree")]
pub mod tree;


#[derive(Debug, Copy, Clone)]
pub enum SignatureDecodeError {
    UnterminatedTypeVar,
    UnexpectedCharacter
}


pub type Result<T> = ::std::result::Result<T, SignatureDecodeError>;

pub struct SignatureReader<'a> {
    data: &'a str
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConstraintKind {
    Unbound, 
    Extends,
    Super,
    Instanceof
}

fn find(haystack: &[u8], from: usize, needle: u8) -> Option<usize> {
    for i in from..haystack.len() {
        if haystack[i] == needle {
            return Some(i);
        }
    }
    None
}
fn flat_map<T, R, F>(opt: Option<T>, f: F) -> Option<R> where F: FnOnce(T) -> Option<R> {
    match opt {
        Some(v) => {
            f(v)
        },
        None => {
            None
        }
    }
}

pub trait SignatureVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn SignatureVisitor> {
        None
    }
    fn visit_formal_type_parameter(&mut self, name: &str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_formal_type_parameter(name);
        }
    }
    fn visit_base_type(&mut self, ty: char) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_base_type(ty);
        }
    }
    fn visit_array_type(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_array_type()
        } else {
            None
        }
    }
    fn visit_superclass(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_superclass()
        } else {
            None
        }
    }
    fn visit_interface(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_interface()
        } else {
            None
        }
    }
    fn visit_class_bound(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_class_bound()
        } else {
            None
        }
    }
    fn visit_interface_bound(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_interface_bound()
        } else {
            None
        }
    }
    fn visit_type_variable(&mut self, name: &str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_variable(name);
        }
    }
    fn visit_inner_class_type(&mut self, name: &str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_inner_class_type(name);
        }
    }
    fn visit_parameter_type(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_parameter_type()
        } else {
            None
        }
    }
    fn visit_return_type(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_return_type()
        } else {
            None
        }
    }
    fn visit_exception_type(&mut self) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_exception_type()
        } else {
            None
        }
    }
    fn visit_class_type(&mut self, name: &str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_class_type(name);
        }
    }
    fn visit_end(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_end();
        }
    }
    fn visit_unbound_type_argument(&mut self) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_unbound_type_argument();
        }
    }
    fn visit_type_argument(&mut self, arg: TypeConstraintKind) -> Option<&mut dyn SignatureVisitor> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_argument(arg)
        } else {
            None
        }
    }
}

impl<'a> SignatureReader<'a> {
    pub fn new(data: &str) -> SignatureReader {
        SignatureReader {
            data
        }
    }
    pub fn accept_type(&self, vis: &mut dyn SignatureVisitor) -> Result<()> {
        self.read_type(0, Some(vis)).map(|_| ())
    }
    pub fn accept(&self, vis: &mut dyn SignatureVisitor) -> Result<()> {
        let bytes = self.data.as_bytes();
        let mut ptr;
        if bytes[0] == b'<' {
            ptr = 1;
            while bytes[ptr] != b'>' {
                let rend = find(bytes, ptr, b':').ok_or(SignatureDecodeError::UnterminatedTypeVar)?;
                vis.visit_formal_type_parameter(&self.data[ptr..rend]);
                ptr = rend + 1;
                if bytes[ptr] == b'L' || bytes[ptr] == b'T' || bytes[ptr] == b'[' {
                    ptr = self.read_type(ptr, vis.visit_class_bound())?;
                }
                while bytes[ptr] == b':' {
                    ptr += 1;
                    ptr = self.read_type(ptr, vis.visit_interface_bound())?;
                }
            }
        } else {
            ptr = 0;
        }
        if bytes[ptr] == b'(' {
            ptr += 1;
            while bytes[ptr] != b')' {
                ptr = self.read_type(ptr, vis.visit_parameter_type())?;
            }
            ptr = self.read_type(ptr + 1, vis.visit_return_type())?;
            while ptr < self.data.len() {
                ptr = self.read_type(ptr + 1, vis.visit_return_type())?;
            }
        } else {
            ptr = self.read_type(ptr, vis.visit_superclass())?;
            while ptr < self.data.len() {
                ptr = self.read_type(ptr, vis.visit_interface())?;
            }
        }
        Ok(())
    }
    fn read_type(&self, mut start: usize, mut vis: Option<&mut dyn SignatureVisitor>) -> Result<usize> {
        let bytes = self.data.as_bytes();
        let cc = bytes[start];
        start += 1;
        match cc {
            b'Z' | b'B' | b'C' | b'S' | b'I' | b'F' | b'J' | b'D' | b'V' => {
                vis.map(|v| v.visit_base_type(cc as char));
                Ok(start)
            },
            b'[' => {
                self.read_type(start, flat_map(vis, |v| v.visit_array_type()))
            },
            b'T' => {
                let rend = find(bytes, start, b';').ok_or(SignatureDecodeError::UnterminatedTypeVar)?;
                vis.map(|v| v.visit_type_variable(&self.data[start..rend]));
                Ok(rend + 1)
            },
            b'L' => {
                let mut visited = false;
                let mut inner = false;
                let mut cname_start = start;
                loop {
                    let cc = bytes[start];
                    start += 1;
                    if cc == b'.' || cc == b';' {
                        if !visited {
                            let name = &self.data[cname_start..start - 1];
                            if inner {
                                vis.as_mut().map(|v| v.visit_inner_class_type(name));
                            } else {
                                vis.as_mut().map(|v| v.visit_class_type(name));
                            }
                        }
                        if cc == b';' {
                            vis.map(|v| v.visit_end());
                            break;
                        }
                        cname_start = start;
                        visited = false;
                        inner = true;
                    } else if cc == b'<' {
                        let name = &self.data[cname_start..start - 1];
                        if inner {
                            vis.as_mut().map(|v| v.visit_inner_class_type(name));
                        } else {
                            vis.as_mut().map(|v| v.visit_class_type(name));
                        }
                        visited = true;
                        loop {
                            let cc = bytes[start] ;
                            if cc == b'>' {
                                break
                            }
                            match cc {
                                b'*' => {
                                    start += 1;
                                    vis.as_mut().map(|v| v.visit_unbound_type_argument());
                                    break;
                                },
                                b'+' => {
                                    start = self.read_type(start + 1, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Extends)))?;
                                },
                                b'-' => {
                                    start = self.read_type(start + 1, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Super)))?;
                                },
                                _ => {
                                    start = self.read_type(start, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Instanceof)))?;
                                }
                            }
                        }
                    }
                }
                Ok(start)
            },
            _ => {
                Err(SignatureDecodeError::UnexpectedCharacter)
            }
        }
    }
}