#[cfg(feature = "signature_tree")]
pub mod tree;


#[derive(Debug, Copy, Clone)]
pub enum SignatureDecodeError {
    UnterminatedTypeVar,
    UnexpectedCharacter
}

pub type Result<T> = ::std::result::Result<T, SignatureDecodeError>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConstraintKind {
    Unbound, 
    Extends,
    Super,
    Instanceof
}

fn find(haystack: &[u8], from: usize, needle: u8) -> Option<usize> {
    for (i, item) in haystack.iter().enumerate().skip(from) {
        if *item == needle {
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

pub trait SignatureVisitor<'a> {
    fn get_wrapped_visitor<'b>(&'b self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        None
    }
    fn visit_formal_type_parameter(&mut self, name: &'a str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_formal_type_parameter(name);
        }
    }
    fn visit_base_type(&mut self, ty: char) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_base_type(ty);
        }
    }
    fn visit_array_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_array_type()
        } else {
            None
        }
    }
    fn visit_superclass<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_superclass()
        } else {
            None
        }
    }
    fn visit_interface<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_interface()
        } else {
            None
        }
    }
    fn visit_class_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_class_bound()
        } else {
            None
        }
    }
    fn visit_interface_bound<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_interface_bound()
        } else {
            None
        }
    }
    fn visit_type_variable(&mut self, name: &'a str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_variable(name);
        }
    }
    fn visit_inner_class_type(&mut self, name: &'a str) {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_inner_class_type(name);
        }
    }
    fn visit_parameter_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_parameter_type()
        } else {
            None
        }
    }
    fn visit_return_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_return_type()
        } else {
            None
        }
    }
    fn visit_exception_type<'b>(&'b mut self) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_exception_type()
        } else {
            None
        }
    }
    fn visit_class_type(&mut self, name: &'a str) {
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
    fn visit_type_argument<'b>(&'b mut self, arg: TypeConstraintKind) -> Option<&'b mut dyn SignatureVisitor<'a>> {
        if let Some(vis) = self.get_wrapped_visitor() {
            vis.visit_type_argument(arg)
        } else {
            None
        }
    }
}

pub fn parse_type<'b>(sig: &'b str, vis: &mut dyn SignatureVisitor<'b>) -> Result<()> {
    read_type(sig, 0, Some(vis)).map(|_| ())
}
pub fn parse<'b>(sig: &'b str, vis: &mut dyn SignatureVisitor<'b>) -> Result<()> {
    let bytes = sig.as_bytes();
    let mut ptr;
    if bytes[0] == b'<' {
        ptr = 1;
        while bytes[ptr] != b'>' {
            let rend = find(bytes, ptr, b':').ok_or(SignatureDecodeError::UnterminatedTypeVar)?;
            vis.visit_formal_type_parameter(&sig[ptr..rend]);
            ptr = rend + 1;
            if bytes[ptr] == b'L' || bytes[ptr] == b'T' || bytes[ptr] == b'[' {
                ptr = read_type(sig, ptr, vis.visit_class_bound())?;
            }
            while bytes[ptr] == b':' {
                ptr += 1;
                ptr = read_type(sig, ptr, vis.visit_interface_bound())?;
            }
        }
    } else {
        ptr = 0;
    }
    if bytes[ptr] == b'(' {
        ptr += 1;
        while bytes[ptr] != b')' {
            ptr = read_type(sig, ptr, vis.visit_parameter_type())?;
        }
        ptr = read_type(sig, ptr + 1, vis.visit_return_type())?;
        while ptr < sig.len() {
            ptr = read_type(sig, ptr + 1, vis.visit_return_type())?;
        }
    } else {
        ptr = read_type(sig, ptr, vis.visit_superclass())?;
        while ptr < sig.len() {
            ptr = read_type(sig, ptr, vis.visit_interface())?;
        }
    }
    Ok(())
}
fn read_type<'b>(sig: &'b str, mut start: usize, mut vis: Option<&mut dyn SignatureVisitor<'b>>) -> Result<usize> {
    let bytes = sig.as_bytes();
    let cc = bytes[start];
    start += 1;
    match cc {
        b'Z' | b'B' | b'C' | b'S' | b'I' | b'F' | b'J' | b'D' | b'V' => {
            if let Some(v) = vis {
                v.visit_base_type(cc as char)
            }
            Ok(start)
        },
        b'[' => {
            read_type(sig, start, flat_map(vis, |v| v.visit_array_type()))
        },
        b'T' => {
            let rend = find(bytes, start, b';').ok_or(SignatureDecodeError::UnterminatedTypeVar)?;
            if let Some(v) = vis {
                v.visit_type_variable(&sig[start..rend]);
            }
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
                        let name = &sig[cname_start..start - 1];
                        if inner {
                            if let Some(v) = vis.as_mut() {
                                v.visit_inner_class_type(name);
                            }
                        } else if let Some(v) = vis.as_mut() {
                            v.visit_class_type(name);
                        }
                    }
                    if cc == b';' {
                        if let Some(v) = vis.as_mut() {
                            v.visit_end();
                        }
                        break;
                    }
                    cname_start = start;
                    visited = false;
                    inner = true;
                } else if cc == b'<' {
                    let name = &sig[cname_start..start - 1];
                    if inner {
                        if let Some(v) = vis.as_mut() {
                            v.visit_inner_class_type(name);
                        }
                    } else if let Some(v) = vis.as_mut() {
                        v.visit_class_type(name);
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
                                if let Some(v) = vis.as_mut() {
                                    v.visit_unbound_type_argument();
                                }
                                break;
                            },
                            b'+' => {
                                start = read_type(sig, start + 1, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Extends)))?;
                            },
                            b'-' => {
                                start = read_type(sig, start + 1, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Super)))?;
                            },
                            _ => {
                                start = read_type(sig, start, flat_map(vis.as_mut(), |v| v.visit_type_argument(TypeConstraintKind::Instanceof)))?;
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