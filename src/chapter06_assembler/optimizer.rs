use crate::chapter06_assembler::parser::{Comp, Dest, Instruction, Jump, Parser};

use Instruction::*;

trait PeepholeRule<'s> {
    fn pattern_length(&self) -> usize;
    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>>;
}

pub struct PeepholeOptimizer<'s> {
    rules: Vec<Box<dyn PeepholeRule<'s>>>,
}

impl<'s> Default for PeepholeOptimizer<'s> {
    fn default() -> Self {
        PeepholeOptimizer {
            rules: vec![Box::new(IndirectAssignment)],
        }
    }
}

impl<'s> PeepholeOptimizer<'s> {
    pub fn optimize(&self, ops: impl IntoIterator<Item = Instruction<'s>>) -> Vec<Instruction<'s>> {
        let mut result = vec![];
        for op in ops {
            result.push(op);
            let mut converged = false;
            while !converged {
                converged = true;
                for rule in &self.rules {
                    if result.len() < rule.pattern_length() {
                        continue;
                    }

                    let pattern_start_index = result.len() - rule.pattern_length();

                    if let Some(new_ops) = rule.apply(&result[pattern_start_index..]) {
                        result.truncate(pattern_start_index);
                        result.extend(new_ops);
                        converged = false;
                    }
                }
            }
        }
        result
    }

    pub fn optimize_str(&self, asm_code: &'s str) -> Result<String, String> {
        let mut ops = vec![];
        let mut parser = Parser::new(asm_code);
        while parser.has_more_lines() {
            parser.advance();
            ops.push(parser.instruction()?);
        }

        let opt = self.optimize(ops);

        let asm_lines: Vec<_> = opt.into_iter().map(|op| op.to_string()).collect();
        Ok(asm_lines.join("\n"))
    }
}

struct IndirectAssignment;

impl<'s> PeepholeRule<'s> for IndirectAssignment {
    fn pattern_length(&self) -> usize {
        3
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [C(Dest::A, comp, Jump::None), C(Dest::D, Comp::A, Jump::None), next @ (A(_) | C(Dest::A, _, _))] => {
                Some(vec![C(Dest::D, *comp, Jump::None), *next])
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indirect_assignment() {
        let code = vec![
            C(Dest::A, Comp::M, Jump::None), // A3 = *A2
            C(Dest::D, Comp::A, Jump::None), // D0 = A3
            A("ARG"),
        ];

        let opt = PeepholeOptimizer::default();
        let result = opt.optimize(code);

        assert_eq!(result, [C(Dest::D, Comp::M, Jump::None), A("ARG"),]);
    }
}
