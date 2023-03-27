use std::mem;
use xtree::{tr, Tree, Cursor, CursorMut};

#[derive(Debug)]
enum TokenType {
    OpeningBracket,
    ClosingBracket,
    Operator,
    Number,
}

fn main() {
    // let mut infix = String::new();
    // println!("Enter infix statement: ");
    // std::io::stdin().read_line(&mut infix).unwrap();

    let operators: Vec<char> = vec!['+', '-', '*', '/', '^'];

    let infix = "((31 * 12) / (4+1))";
    let infix = infix.split_whitespace().collect::<Vec<&str>>().join("");
    println!("{}", infix);

    let mut tokens: Vec<(TokenType, String)> = Vec::new();
    let mut infix_chars = infix.char_indices().peekable();

    while let Some((index, char)) = infix_chars.next() {
        if operators.contains(&char) {
            tokens.push((TokenType::Operator, char.to_string()))
        } else if char == '(' {
            tokens.push((TokenType::OpeningBracket, char.to_string()))
        } else if char == ')' {
            tokens.push((TokenType::ClosingBracket, char.to_string()))
        } else if char.is_numeric() {
            while let Some((index_2, next_char)) = infix_chars.clone().peek() {
                if next_char.is_numeric() {
                    infix_chars.next();
                } else {
                    tokens.push((TokenType::Number, infix[index..*index_2].to_string()));
                    break;
                }
            }
        }
    }

    let mut tree = tr!("".to_string());
    let mut current = tree.cursor_mut();

    for _ in 0..tokens.len() {
        current.add_child(tr!("".to_string()));
        current.move_child(0);
    }

    for (token_type, token) in tokens.iter() {
        match token_type {
            TokenType::OpeningBracket => {
                current.add_child(tr!("".to_string()));
                current.move_child(0)
            }
            TokenType::ClosingBracket => {
                current.move_parent()
            }
            TokenType::Operator => {
                println!("OP: {}", token);
                *current.current() = token.clone();
                current.add_child(tr!("".to_string()));
                current.move_child(1)
            }
            TokenType::Number => {
                *current.current() = token.clone();

                if current.is_root() {
                    tree = tr!("".to_string()) / tree.clone();

                    current = tree.cursor_mut();
                } else {
                    current.move_parent()
                }
            }
        }
    }
    
    for token in tree.df_iter() {
        if token != "" {
            println!("{}", token)
        }
    }
}