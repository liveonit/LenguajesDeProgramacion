use std::io::{self, BufRead};
use std::*;

fn main() {
    let mut lines = io::stdin().lock().lines();
    let mut vec: Vec<f64> = vec![];

    while let Some(line) = lines.next() {
        let last_input = line.unwrap();
        if last_input.len() == 0 {
          if vec.len() == 0 {
            break;
          }
          else {
            println!("{:?}", vec.pop())
          }
        } else {
          last_input.split_whitespace().for_each(|num| vec.push(num.parse::<f64>().unwrap()));
        }
      }
}
