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
            rules: vec![
                Box::new(IndirectAssignment),
                Box::new(TrivialConstant4),
                Box::new(TrivialConstant5),
            ],
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

struct TrivialConstant4;

impl<'s> PeepholeRule<'s> for TrivialConstant4 {
    fn pattern_length(&self) -> usize {
        4
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [A(c @ ("0" | "1" | "-1")), C(Dest::D, Comp::A, Jump::None), A(ptr), C(dest, Comp::D, jmp)] =>
            {
                let comp = match *c {
                    "0" => Comp::Zero,
                    "1" => Comp::One,
                    "-1" => Comp::MinusOne,
                    _ => unreachable!(),
                };
                Some(vec![A(ptr), C(*dest, comp, *jmp)])
            }
            _ => None,
        }
    }
}

struct TrivialConstant5;

impl<'s> PeepholeRule<'s> for TrivialConstant5 {
    fn pattern_length(&self) -> usize {
        5
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [A(c @ ("0" | "1" | "-1")), C(Dest::D, Comp::A, Jump::None), A(ptr), middle @ C(
                Dest::A | Dest::M | Dest::AM,
                Comp::M
                | Comp::A
                | Comp::IncM
                | Comp::IncA
                | Comp::DecM
                | Comp::DecA
                | Comp::NegA
                | Comp::NegM
                | Comp::NotA
                | Comp::NotM
                | Comp::One
                | Comp::Zero
                | Comp::MinusOne,
                Jump::None,
            ), C(dest, Comp::D, jmp)] => {
                let comp = match *c {
                    "0" => Comp::Zero,
                    "1" => Comp::One,
                    "-1" => Comp::MinusOne,
                    _ => unreachable!(),
                };
                Some(vec![A(ptr), *middle, C(*dest, comp, *jmp)])
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
            C(Dest::A, Comp::M, Jump::None),
            C(Dest::D, Comp::A, Jump::None),
            A("ARG"),
        ];

        let opt = PeepholeOptimizer::default();
        let result = opt.optimize(code);

        assert_eq!(result, [C(Dest::D, Comp::M, Jump::None), A("ARG"),]);
    }

    #[test]
    fn trivial_constant4() {
        let code = vec![
            A("0"),
            C(Dest::D, Comp::A, Jump::None),
            A("ARG"),
            C(Dest::M, Comp::D, Jump::None),
        ];

        let opt = PeepholeOptimizer::default();
        let result = opt.optimize(code);

        assert_eq!(result, [A("ARG"), C(Dest::M, Comp::Zero, Jump::None),]);
    }

    #[test]
    fn trivial_constant5() {
        let code = vec![
            A("0"),
            C(Dest::D, Comp::A, Jump::None),
            A("ARG"),
            C(Dest::A, Comp::IncM, Jump::None),
            C(Dest::M, Comp::D, Jump::None),
        ];

        let opt = PeepholeOptimizer::default();
        let result = opt.optimize(code);

        assert_eq!(
            result,
            [
                A("ARG"),
                C(Dest::A, Comp::IncM, Jump::None),
                C(Dest::M, Comp::Zero, Jump::None),
            ]
        );
    }
}
