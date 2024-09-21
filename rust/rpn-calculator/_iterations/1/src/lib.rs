#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack = Vec::new();
    for input in inputs {
        if let CalculatorInput::Value(val) = input {
            stack.push(*val)
        } else {
            let right = stack.pop()?;
            let left = stack.pop()?;
            let result = match input {
                CalculatorInput::Add => left + right,
                CalculatorInput::Subtract => left - right,
                CalculatorInput::Multiply => left * right,
                CalculatorInput::Divide => left / right,
                CalculatorInput::Value(_) => unreachable!()
            };
            stack.push(result);
        }
    }
    if stack.len() == 1 {
        stack.pop()
    } else {
        None
    }
}
