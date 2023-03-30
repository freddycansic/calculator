#![feature(is_some_and, let_chains)]

use std::{collections::VecDeque, fmt::Debug, mem::discriminant};

use colored::Colorize;
use derive_more::Constructor;
use failure::{format_err, Error};
use ptree::{print_tree, TreeBuilder};
use r3bl_rs_utils::Arena;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    OpeningBracket,
    ClosingBracket,
    Operator(Operator),
    Number(f32),
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::ClosingBracket => ")".to_string(),
            Token::OpeningBracket => "(".to_string(),
            Token::Number(num) => num.to_string(),
            Token::Operator(op) => op.symbol.to_string(),
        }
    }
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

    let tokens = tokenise(&infix, &operators).unwrap_or_else(|err| panic!("{}", err));
    let rpn_tokens = shunting_yard(&tokens).unwrap_or_else(|err| panic!("{}", err));

    println!(
        "{}",
        rpn_tokens.iter().fold(String::new(), |mut str, token| {
            str += &(token.to_string() + " ").to_string();
            str
        })
    );

    print_arena_tree(&string_tree_from_rpn(&rpn_tokens).unwrap_or_else(|err| panic!("{}", err)))
        .unwrap_or_else(|err| panic!("{}", err));

    println!(
        "{}",
        evaluate_rpn(&rpn_tokens).unwrap_or_else(|err| panic!("{}", err))
    );
}

fn tokenise(infix: &str, operators: &[Operator]) -> Result<Vec<Token>, Error> {
    let infix = infix.split_whitespace().collect::<Vec<&str>>().join("");

    let mut tokens: Vec<Token> = Vec::new();
    let mut infix_chars = infix.char_indices();

    while let Some((index, char)) = infix_chars.next() {
        if let Some(op) = operators
            .iter()
            .find_map(|op| (op.symbol == char).then_some(op))
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
                infix[index..next_non_num_index]
                    .parse::<f32>()
                    .map_err(|_| {
                        format_err!(
                            "{} Failed to cast a token to a float.",
                            "ERROR!".red().bold()
                        )
                    })?,
            ));
        } else {
            Err(format_err!(
                "{} Could not tokenise input! Unexpected token \"{}\"!",
                "ERROR!".red().bold(),
                char.to_string().yellow()
            ))?
        }
    }

    Ok(tokens)
}

fn evaluate_rpn(rpn_tokens: &Vec<Token>) -> Result<f32, Error> {
    let mut stack = Vec::<f32>::new();
    for token in rpn_tokens.iter() {
        match token {
            Token::Operator(op) => {
                let operand_1 = stack.pop().ok_or(format_err!(
                    "{} Not enough operands for operator \"{}\"!",
                    "ERROR!".red().bold(),
                    op.symbol.to_string().yellow()
                ))?;
                let operand_2 = stack.pop().ok_or(format_err!(
                    "{} Not enough operands for operator \"{}\"!",
                    "ERROR!".red().bold(),
                    op.symbol.to_string().yellow()
                ))?;

                match op.symbol {
                    '+' => stack.push(operand_2 + operand_1),
                    '-' => stack.push(operand_2 - operand_1),
                    '*' => stack.push(operand_2 * operand_1),
                    '/' => stack.push(operand_2 / operand_1),
                    '^' => stack.push(operand_2.powf(operand_1)),
                    '%' => stack.push(operand_2 % operand_1),
                    _ => panic!(),
                }
            }
            Token::Number(num) => stack.push(*num),
            _ => Err(format_err!(
                "{} Could not evaluate RPN expression!",
                "ERROR!".red().bold()
            ))?,
        }
    }

    Ok(stack.pop().ok_or(format_err!(
        "{} Could not evaluate RPN expression!",
        "ERROR!".red().bold()
    ))?)
}

fn shunting_yard(tokens: &Vec<Token>) -> Result<Vec<Token>, Error> {
    let mut rpn_tokens = Vec::<Token>::new();
    let mut stack = VecDeque::<Token>::new();

    for token in tokens.iter() {
        match token {
            Token::Operator(operator_x) => {
                // if the top of stack is an operator type. discriminant means disregard the contents of the enum and only compare the types
                while let Some(Token::Operator(operator_y)) = stack.back() {
                    if discriminant(stack.back().unwrap()) == discriminant(token)
                        && ((operator_x.associativity == Associativity::Left
                            && operator_x.precedence <= operator_y.precedence)
                            || (operator_x.associativity == Associativity::Right
                                && operator_x.precedence < operator_y.precedence))
                    {
                        rpn_tokens.push(stack.pop_back().ok_or(format_err!(
                            "{} Could not parse expression to RPN!",
                            "ERROR!".red().bold()
                        ))?);
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

    Ok(rpn_tokens)
}

fn find_root_node<T>(tree: &Arena<T>) -> Option<usize>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let mut current_node_id = 0;
    while tree.has_parent(current_node_id) {
        current_node_id += 1;
    }

    tree.node_exists(current_node_id).then_some(current_node_id)
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

fn print_arena_tree<T>(tree: &Arena<T>) -> Result<(), Error>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let root_node = find_root_node(&tree).ok_or(failure::format_err!(
        "{} No root node found.",
        "ERROR!".red().bold()
    ))?;

    let mut builder = TreeBuilder::new("Tree".to_string());
    build_string_tree(&mut builder, tree, root_node);
    let output = builder.build();
    print_tree(&output)?;

    Ok(())
}

fn string_tree_from_rpn(rpn_tokens: &Vec<Token>) -> Result<Arena<String>, Error> {
    let mut tree = Arena::<String>::new();
    let mut stack = Vec::<usize>::new();

    for token in rpn_tokens.iter() {
        match token {
            Token::Operator(op) => {
                let operand_1_id = stack.pop().ok_or(format_err!(
                    "{} Not enough operands for operator \"{}\"!",
                    "ERROR!".red().bold(),
                    op.symbol.to_string().yellow()
                ))?;
                let operand_2_id = stack.pop().ok_or(format_err!(
                    "{} Not enough operands for operator \"{}\"!",
                    "ERROR!".red().bold(),
                    op.symbol.to_string().yellow()
                ))?;

                let operator_root_id = tree.add_new_node(op.symbol.to_string(), None);
                tree.get_node_arc(operator_root_id)
                    .unwrap()
                    .write()
                    .unwrap()
                    .children_ids = [operand_1_id, operand_2_id].into();

                tree.get_node_arc(operand_1_id)
                    .unwrap()
                    .write()
                    .unwrap()
                    .parent_id = Some(operator_root_id);
                tree.get_node_arc(operand_2_id)
                    .unwrap()
                    .write()
                    .unwrap()
                    .parent_id = Some(operator_root_id);

                stack.push(operator_root_id)
            }
            Token::Number(num) => stack.push(tree.add_new_node(num.to_string(), None)),
            _ => {
                return Err(format_err!(
                    "{} Unexpected token : \"{}\"",
                    "ERROR!".red().bold(),
                    token.to_string().yellow()
                ))
            }
        }
    }

    Ok(tree)
}