use inkwell::values::AnyValueEnum;

#[derive(Debug, PartialEq)]
pub struct BackendError {
    pub message: &'static str, //TODO: Fix this to be String, but better if we use enums...
}

pub type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;