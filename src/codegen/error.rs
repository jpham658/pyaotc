use inkwell::values::AnyValueEnum;

#[derive(Debug, PartialEq)]
pub struct BackendError {
    pub message: &'static str,
}

pub type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;