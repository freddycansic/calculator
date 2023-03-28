#![feature(let_chains)]

use std::{collections::VecDeque, fmt::Debug, path::Iter};

use r3bl_rs_utils::{Arena, NodeRef};

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

    let mut tree = Arena::<String>::new();
    let mut current = 0;
    tree.add_new_node(String::default(), None);

    for (token_type, token) in tokens.iter() {
        match token_type {
            TokenType::OpeningBracket => {
                current = tree.add_new_node(String::default(), Some(current));
            }
            TokenType::ClosingBracket => {
                current = create_parent_if_none(&mut tree, current);
            }
            TokenType::Operator => {
                tree.get_node_arc(current).unwrap().write().unwrap().payload = token.clone();
                current = tree.add_new_node(String::default(), Some(current));
            }
            TokenType::Number => {
                tree.get_node_arc(current).unwrap().write().unwrap().payload = token.clone();
                current = create_parent_if_none(&mut tree, current);
            }
        }
    }

    // for el in tree.tree_walk_bfs(0).unwrap() {
    //     println!("{}", tree.get_node_arc(el).unwrap().read().unwrap().payload)
    // }
    postfix_traversal(&tree, 0);
}

fn postfix_traversal<T>(tree: &Arena<T>, mut root: usize) -> ()
where
    T: Default + Clone + Debug + Send + Sync,
{
    //     Create an empty stack and push the root node onto the stack.
    // While the stack is not empty:
    // a) Pop the top node from the stack.
    // b) If the popped node has both left and right children, push the right child onto the stack followed by the left child.
    // c) If the popped node has no children or both its children have been visited, print its value.
    // d) If the popped node has only a left child and the left child has not been visited, push the left child onto the stack.
    // e) If the popped node has only a right child and the right child has not been visited, push the right child onto the stack.
    // f) Mark the popped node as visited.
    // Repeat step 2 until the stack is empty.

    if let None = tree.get_node_arc(root) {
        panic!("No node at {root} to traverse from!");
    }

    println!();

    let mut stack = VecDeque::<usize>::new();
    let mut visited = Vec::<usize>::new();

    stack.push_back(root);

    while !stack.is_empty() {
        let node = stack.pop_back().unwrap();

        match tree.get_children_of(node) {
            Some(children) => match children.len() {
                2 => {
                    let left_child = children.front().unwrap().clone();
                    let right_child = children.back().unwrap().clone();

                    let left_child_visited = visited.contains(&left_child);
                    let right_child_visited = visited.contains(&right_child);

                    if left_child_visited && right_child_visited {
                        println!(
                            "{:#?}",
                            tree.get_node_arc(node).unwrap().read().unwrap().payload
                        );
                    } else if !left_child_visited && !right_child_visited {
                        stack.push_back(right_child);
                        stack.push_back(left_child);
                    }
                }
                1 => {
                    let single_child = children.front().unwrap().clone();
                    if !visited.contains(&single_child) {
                        stack.push_back(single_child)
                    }
                }
                _ => (),
            },
            None => {
                println!(
                    "{:#?}",
                    tree.get_node_arc(node).unwrap().read().unwrap().payload
                );
            }
        }

        visited.push(node);
    }
    println!();
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
