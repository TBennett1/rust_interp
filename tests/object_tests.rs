use rust_interp::object::Object;

#[test]
fn string_hash_key() {
    let hello1 = Object::String(String::from("Hello World"));
    let hello2 = Object::String(String::from("Hello World"));
    let diff1 = Object::String(String::from("My name is johnny"));
    let diff2 = Object::String(String::from("My name is johnny"));

    if Object::hash_key(hello1.clone()) != Object::hash_key(hello2) {
        panic!("strings with same content have different hash keys")
    }
    if Object::hash_key(diff1.clone()) != Object::hash_key(diff2) {
        panic!("strings with same content have different hash keys")
    }
    if Object::hash_key(hello1) == Object::hash_key(diff1) {
        panic!("strings with different content have same hash keys")
    }
}

#[test]
fn integer_hash_key() {
    let int1 = Object::Integer(10);
    let int2 = Object::Integer(10);
    let diffint1 = Object::Integer(20);
    let diffint2 = Object::Integer(20);

    if Object::hash_key(int1.clone()) != Object::hash_key(int2) {
        panic!("ints with same content have different hash keys")
    }
    if Object::hash_key(diffint1.clone()) != Object::hash_key(diffint2) {
        panic!("ints with same content have different hash keys")
    }
    if Object::hash_key(int1) == Object::hash_key(diffint1) {
        panic!("ints with different content have same hash keys")
    }
}

#[test]
fn boolean_hash_keys() {
    let bool1 = Object::Boolean(true);
    let bool2 = Object::Boolean(true);
    let diffbool1 = Object::Boolean(false);
    let diffbool2 = Object::Boolean(false);

    if Object::hash_key(bool1.clone()) != Object::hash_key(bool2) {
        panic!("ints with same content have different hash keys")
    }
    if Object::hash_key(diffbool1.clone()) != Object::hash_key(diffbool2) {
        panic!("ints with same content have different hash keys")
    }
    if Object::hash_key(bool1) == Object::hash_key(diffbool1) {
        panic!("ints with different content have same hash keys")
    }
}
