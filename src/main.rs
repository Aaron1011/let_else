use let_else::let_else;

struct MyStruct { field: u8 }

fn foo() {
    let_else!(let Some(MyStruct { field }) = Some(MyStruct { field: 25 }) else return);
    println!("Val: {:?}", field);
}

fn main() {
    foo();
}
