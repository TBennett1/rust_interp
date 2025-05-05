use rust_interp::{lexer::Lexer, token::Token};

#[test]
fn get_next_token() {
    let input = "=+(){},;";
    let mut lexer = Lexer::new(input.into());

    let tokens = vec![
        Token::Assign,
        Token::Plus,
        Token::Lparen,
        Token::Rparen,
        Token::Lbrace,
        Token::Rbrace,
        Token::Comma,
        Token::Semicolon,
    ];

    for token in tokens {
        let next_token = lexer.next_token().unwrap();
        println!("expected: {:?}, recieved: {:?}", token, next_token);
        assert_eq!(token, next_token);
    }
}

#[test]
fn test_next_token() {
    let input = r#"let five = 5;
    let ten = 10;
    
    let add = fn(x, y) {
         x + y;
    };
    
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }
    
    10 == 10; 
    10 != 9;
    "foobar"
    "foo bar"
    [1, 2];
    {"foo": "bar"}
    "#;

    let mut lex = Lexer::new(input.into());

    let tokens = vec![
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("add")),
        Token::Assign,
        Token::Function,
        Token::Lparen,
        Token::Ident(String::from("x")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::Rparen,
        Token::Lbrace,
        Token::Ident(String::from("x")),
        Token::Plus,
        Token::Ident(String::from("y")),
        Token::Semicolon,
        Token::Rbrace,
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("result")),
        Token::Assign,
        Token::Ident(String::from("add")),
        Token::Lparen,
        Token::Ident(String::from("five")),
        Token::Comma,
        Token::Ident(String::from("ten")),
        Token::Rparen,
        Token::Semicolon,
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int(5),
        Token::Semicolon,
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::Gt,
        Token::Int(5),
        Token::Semicolon,
        Token::If,
        Token::Lparen,
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::Rparen,
        Token::Lbrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::Rbrace,
        Token::Else,
        Token::Lbrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::Rbrace,
        Token::Int(10),
        Token::Eq,
        Token::Int(10),
        Token::Semicolon,
        Token::Int(10),
        Token::NotEq,
        Token::Int(9),
        Token::Semicolon,
        Token::String("foobar".to_string()),
        Token::String("foo bar".to_string()),
        Token::Lbracket,
        Token::Int(1),
        Token::Comma,
        Token::Int(2),
        Token::Rbracket,
        Token::Semicolon,
        Token::Lbrace,
        Token::String("foo".to_string()),
        Token::Colon,
        Token::String("bar".to_string()),
        Token::Rbrace,
        Token::Eof,
    ];

    for token in tokens {
        let next_token = lex.next_token().unwrap();
        println!("expected: {:?}, recieved: {:?}", token, next_token);
        assert_eq!(token, next_token);
    }
}
