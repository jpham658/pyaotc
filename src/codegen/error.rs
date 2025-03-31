use inkwell::values::AnyValueEnum;

#[derive(Debug, PartialEq)]
pub struct BackendError {
    pub message: String, 
}

pub type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;