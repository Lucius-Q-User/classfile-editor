#[cfg(feature = "signature_tree")]
pub mod tree;

pub struct SignatureReader<'a> {
    data: &'a str
}

pub trait SignatureVisitor {
    fn get_wrapped_visitor(&self) -> Option<&mut dyn SignatureVisitor> {
        None
    }
}

impl<'a> SignatureReader<'a> {
    pub fn new(data: &str) -> SignatureReader {
        SignatureReader {
            data
        }
    }
    pub fn accept_method(&self, vis: &mut dyn SignatureVisitor) {
        let mut iter = self.data.bytes();
        while ite
    }
}