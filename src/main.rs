#![feature(let_chains)]

use std::{collections::VecDeque, fmt::Debug, io};

use ptree::{print_tree, TreeBuilder};
use r3bl_rs_utils::Arena;

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
    // println!("{}", infix);

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
    // println!("{:#?}", postfix_traversal(&tree));

    if let Err(err) = print_arena_tree(&tree) {
        panic!("{}", err);
    }

    println!();
    postfix_traversal(&tree)
        .iter()
        .for_each(|node_id| print!("{}", node_id));
    println!();
}

fn postfix_traversal<T>(tree: &Arena<T>) -> VecDeque<usize>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let mut tree_with_none = Arena::<Option<T>>::new();
    todo!("Copy tree and add null children to leaves");

    if let Err(err) = print_arena_tree(&tree_with_none) {
        panic!("{}", err);
    }

    let mut path = VecDeque::<usize>::new();
    postfix(&tree_with_none, 0, &mut path);
    path
}

fn postfix<T>(tree: &Arena<Option<T>>, current: usize, path: &mut VecDeque<usize>)
where
    T: Default + Clone + Debug + Send + Sync,
{
    if tree
        .get_node_arc(current)
        .unwrap()
        .read()
        .unwrap()
        .payload
        .is_none()
    {
        return;
    }

    postfix(
        tree,
        *tree.get_children_of(current).unwrap().front().unwrap(),
        path,
    );
    postfix(
        tree,
        *tree.get_children_of(current).unwrap().back().unwrap(),
        path,
    );
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

fn build_tree<T>(builder: &mut TreeBuilder, tree: &Arena<T>, current_id: usize)
where
    T: Default + Clone + Debug + Send + Sync,
{
    // builder.begin_child(format!("{:#?}: \"{:#?}\"", current_id, tree.get_node_arc(current_id).unwrap().read().unwrap().payload));
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
            build_tree(builder, tree, child_id);
        }
    }

    builder.end_child();
}

fn print_arena_tree<T>(tree: &Arena<T>) -> io::Result<()>
where
    T: Default + Clone + Debug + Send + Sync,
{
    let mut builder = TreeBuilder::new("Expression".to_string());
    build_tree(&mut builder, tree, 0);
    let output = builder.build();
    print_tree(&output)?;

    Ok(())
}
