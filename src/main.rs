#![feature(is_some_and)]

use std::{collections::VecDeque, fmt::Debug, io};

use derive_more::Constructor;
use ptree::{print_tree, TreeBuilder};
use r3bl_rs_utils::Arena;

#[derive(Debug, Clone, Default, PartialEq)]
enum TokenType {
    OpeningBracket,
    ClosingBracket,
    Operator,
    #[default]
    Number,
}

#[derive(Debug, Clone, Default, Constructor)]
struct Token {
    token_type: TokenType,
    token: String,
}

fn main() {
    // let mut infix = String::new();
    // println!("Enter infix statement: ");
    // std::io::stdin().read_line(&mut infix).unwrap();

    let operators = ['+', '-', '*', '/', '^'];
    let infix = "(((1+2)*4)/3)";
    println!("{}", infix);

    let tokens = tokenise(infix, &operators);

    // if let Err(err) = print_arena_tree(&tree) {
    //     panic!("{}", err);
    // }

    let parse_tree = build_parse_tree(&tokens);
    let postfix_traveral = postfix_traversal(&parse_tree);

    println!("{}", evaluate(&parse_tree, &postfix_traveral));
}

fn build_parse_tree(tokens: &Vec<Token>) -> Arena<Token> {
    let mut tree = Arena::<Token>::new();
    let mut current = 0;
    tree.add_new_node(Token::default(), None);

    for token in tokens.iter() {
        match token.token_type {
            TokenType::OpeningBracket => {
                current = tree.add_new_node(Token::default(), Some(current));
            }
            TokenType::ClosingBracket => {
                current = create_parent_if_none(&mut tree, current);
            }
            TokenType::Operator => {
                tree.get_node_arc(current).unwrap().write().unwrap().payload = token.clone();
                current = tree.add_new_node(Token::default(), Some(current));
            }
            TokenType::Number => {
                tree.get_node_arc(current).unwrap().write().unwrap().payload = token.clone();
                current = create_parent_if_none(&mut tree, current);
            }
        }
    }

    tree

}

fn tokenise(infix: &str, operators: &[char]) -> Vec<Token> {
    let infix = infix.split_whitespace().collect::<Vec<&str>>().join("");

    let mut tokens: Vec<Token> = Vec::new();
    let mut infix_chars = infix.char_indices().peekable();

    while let Some((index, char)) = infix_chars.next() {
        if operators.contains(&char) {
            tokens.push(Token::new(TokenType::Operator, char.to_string()))
        } else if char == '(' {
            tokens.push(Token::new(TokenType::OpeningBracket, char.to_string()))
        } else if char == ')' {
            tokens.push(Token::new(TokenType::ClosingBracket, char.to_string()))
        } else if char.is_numeric() {
            while let Some((index_2, next_char)) = infix_chars.clone().peek() {
                if next_char.is_numeric() {
                    infix_chars.next();
                } else {
                    tokens.push(Token::new(
                        TokenType::Number,
                        infix[index..*index_2].to_string(),
                    ));
                    break;
                }
            }
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


fn evaluate(tree: &Arena<Token>, path: &VecDeque<usize>) -> f32 {
    let mut stack = Vec::<f32>::new();
    for node_id in path.iter() {

        let token = tree
            .get_node_arc(*node_id)
            .unwrap()
            .read()
            .unwrap()
            .payload
            .clone();

        match token.token_type {
            TokenType::Operator => {
                let operand_1 = stack.pop().unwrap();
                let operand_2 = stack.pop().unwrap();

                match token.token.as_str() {
                    "+" => stack.push(operand_2 + operand_1),
                    "-" => stack.push(operand_2 - operand_1),
                    "*" => stack.push(operand_2 * operand_1),
                    "/" => stack.push(operand_2 / operand_1),
                    "^" => stack.push(operand_2.powf(operand_1)),
                    _ => panic!(),
                }
            }
            TokenType::Number => stack.push(token.token.parse::<f32>().unwrap()),
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