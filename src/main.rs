#![feature(is_some_and, let_chains)]

use std::{collections::VecDeque, fmt::Debug, io, mem::discriminant};

use derive_more::Constructor;
use ptree::{print_tree, TreeBuilder};
use r3bl_rs_utils::Arena;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    OpeningBracket,
    ClosingBracket,
    Operator(Operator),
    Number(f32),
}

impl Default for Token {
    fn default() -> Self {
        Token::Number(0.0)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
enum Associativity {
    #[default]
    Left,
    Right,
}

#[derive(Constructor, Clone, Debug, PartialEq)]
struct Operator {
    symbol: char,
    associativity: Associativity,
    precedence: u32,
}

fn main() {
    let mut infix = String::new();
    println!("Enter infix statement: ");
    std::io::stdin().read_line(&mut infix).unwrap();

    let operators = [
        Operator::new('+', Associativity::Left, 0),
        Operator::new('-', Associativity::Left, 0),
        Operator::new('*', Associativity::Left, 1),
        Operator::new('/', Associativity::Left, 1),
        Operator::new('%', Associativity::Left, 1),
        Operator::new('^', Associativity::Right, 2),
    ];

    let tokens = tokenise(&infix, &operators);
    let rpn = shunting_yard(&tokens);
    println!("{}", evaluate_rpn(&rpn));
}

fn tokenise(infix: &str, operators: &[Operator]) -> Vec<Token> {
    let infix = infix.split_whitespace().collect::<Vec<&str>>().join("");

    let mut tokens: Vec<Token> = Vec::new();
    let mut infix_chars = infix.char_indices();

    while let Some((index, char)) = infix_chars.next() {
        if let Some(op) = operators
            .iter()
            .find_map(|op| (op.symbol == char).then(|| op))
        {
            tokens.push(Token::Operator(op.clone()));
        } else if char == '(' {
            tokens.push(Token::OpeningBracket)
        } else if char == ')' {
            tokens.push(Token::ClosingBracket)
        } else if char.is_numeric() {
            let next_non_num_index = match infix
                .chars()
                .skip(index)
                .position(|char| !char.is_numeric())
            {
                Some(next_non_num_index) => next_non_num_index + index,
                None => infix.len(), // if there arent any more chars then end the num
            };

            for _ in 0..next_non_num_index - index - 1 {
                infix_chars.next();
            }

            tokens.push(Token::Number(
                infix[index..next_non_num_index].parse::<f32>().unwrap(),
            ));
        }
    }

    tokens
}

fn postfix_traversal<T>(tree: &Arena<T>) -> VecDeque<usize>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let mut path = VecDeque::<usize>::new();
    postfix(tree, 0, &mut path);
    path
}

fn postfix<T>(tree: &Arena<T>, current: usize, path: &mut VecDeque<usize>)
where
    T: Default + Clone + Debug + Send + Sync,
{
    if let Some(children) = tree.get_children_of(current) {
        for child in children.iter() {
            postfix(tree, *child, path);
        }
    }

    path.push_back(current)
}

fn create_parent_if_none<T>(tree: &mut Arena<T>, current: usize) -> usize
where
    T: Default + Clone + Debug + Send + Sync,
{
    match tree.get_parent_of(current) {
        Some(parent) => parent,
        None => {
            let parent = tree.add_new_node(T::default(), None);
            tree.get_node_arc(current)
                .unwrap()
                .write()
                .unwrap()
                .parent_id = Some(parent);
            tree.get_node_arc(parent)
                .unwrap()
                .write()
                .unwrap()
                .children_ids
                .push_back(current);

            parent
        }
    }
}

fn evaluate_rpn(rpn_tokens: &Vec<Token>) -> f32 {
    let mut stack = Vec::<f32>::new();
    for token in rpn_tokens.iter() {
        match token {
            Token::Operator(operator) => {
                let operand_1 = stack.pop().unwrap();
                let operand_2 = stack.pop().unwrap();

                match operator.symbol {
                    '+' => stack.push(operand_2 + operand_1),
                    '-' => stack.push(operand_2 - operand_1),
                    '*' => stack.push(operand_2 * operand_1),
                    '/' => stack.push(operand_2 / operand_1),
                    '^' => stack.push(operand_1.powf(operand_2)),
                    '%' => stack.push(operand_2 % operand_1),
                    _ => panic!(),
                }
            }
            Token::Number(number) => stack.push(*number),
            _ => panic!(),
        }
    }

    stack.pop().unwrap()
}

fn build_string_tree<T>(builder: &mut TreeBuilder, tree: &Arena<T>, current_id: usize)
where
    T: Default + Clone + Debug + Send + Sync,
{
    // builder.begin_child(format!("{:#?}: {:#?}", current_id, tree.get_node_arc(current_id).unwrap().read().unwrap().payload));
    builder.begin_child(format!(
        "{:?}",
        tree.get_node_arc(current_id)
            .unwrap()
            .read()
            .unwrap()
            .payload
    ));

    if let Some(children_ids) = tree.get_children_of(current_id) {
        for child_id in children_ids {
            build_string_tree(builder, tree, child_id);
        }
    }

    builder.end_child();
}

fn print_arena_tree<T>(tree: &Arena<T>) -> io::Result<()>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let mut builder = TreeBuilder::new("Expression".to_string());
    build_string_tree(&mut builder, tree, 0);
    let output = builder.build();
    print_tree(&output)?;

    Ok(())
}

// For all the input tokens [S1]:
//  Read the next token [S2];
//  If token is an operator (x) [S3]:
//      While there is an operator (y) at the top of the operators stack and either (x) is left-associative and its precedence is less or equal to that of (y), or (x) is right-associative and its precedence is less than (y) [S4]:
//          Pop (y) from the stack [S5];
//          Add (y) output buffer [S6];
//      Push (x) on the stack [S7];
//  Else If token is left parenthesis, then push it on the stack [S8];
//  Else If token is a right parenthesis [S9]:
//      Until the top token (from the stack) is left parenthesis, pop from the stack to the output buffer [S10];
//      Also pop the left parenthesis but don't include it in the output buffer [S11];
//  Else add token to output buffer [S12].
//  While there are still operator tokens in the stack, pop them to output [S13]

fn shunting_yard(tokens: &Vec<Token>) -> Vec<Token> {
    let mut rpn_tokens = Vec::<Token>::new();
    let mut stack = VecDeque::<Token>::new();

    for token in tokens.iter() {
        match token {
            Token::Operator(operator_x) => {
                // if the top of stack is an operator type. discriminant means disregard the contents of the enum
                while let Some(Token::Operator(operator_y)) = stack.back() {
                    if discriminant(stack.back().unwrap()) == discriminant(token)
                        && ((operator_x.associativity == Associativity::Left
                            && operator_x.precedence <= operator_y.precedence)
                            || (operator_x.associativity == Associativity::Right
                                && operator_x.precedence < operator_y.precedence))
                    {
                        rpn_tokens.push(stack.pop_back().unwrap());
                    } else {
                        break;
                    }
                }

                stack.push_back(token.clone());
            }
            Token::OpeningBracket => stack.push_back(token.clone()),
            Token::ClosingBracket => {
                while let Some(token) = stack.pop_back() {
                    if token != Token::OpeningBracket {
                        rpn_tokens.push(token.clone())
                    } else {
                        break;
                    }
                }
            }
            Token::Number(_) => rpn_tokens.push(token.clone()),
        }
    }

    while let Some(token) = stack.pop_back() {
        rpn_tokens.push(token.clone())
    }

    rpn_tokens
}
