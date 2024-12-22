import ply.lex as lex
import ply.yacc as yacc
import math

# 1. Lexical Analysis: Tokenizer
tokens = (
    'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'FACTORIAL',
    'LPAREN', 'RPAREN', 'SIN', 'COS'
)

# Token regex patterns
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER = r'\^'
t_FACTORIAL = r'!'
t_LPAREN = r'\('
t_RPAREN = r'\)'

def t_SIN(t):
    r'sin'
    return t

def t_COS(t):
    r'cos'
    return t

# Token for numbers
def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value)
    return t

# Ignored characters
t_ignore = ' \t'

# Error handling for invalid characters
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# 2. Syntax Analysis: Parser
derivation_steps = []  # To store the derivation steps

def add_derivation_step(rule, result):
    """Adds a derivation step to the global list."""
    derivation_steps.insert(0, f"{rule} → {result}")  # Insert at the start

def p_expression_binop(p):
    '''expression : expression PLUS term
                  | expression MINUS term
                  | expression TIMES term
                  | expression DIVIDE term
                  | expression POWER term'''
    p[0] = ('binop', p[2], p[1], p[3])
    add_derivation_step("expression", f"expression {p[2]} term")

def p_expression_factorial(p):
    'expression : expression FACTORIAL'
    p[0] = ('factorial', p[1])
    add_derivation_step("expression", "expression !")

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]
    add_derivation_step("expression", "( expression )")

def p_expression_function(p):
    '''expression : SIN LPAREN expression RPAREN
                  | COS LPAREN expression RPAREN'''
    p[0] = ('function', p[1], p[3])
    add_derivation_step("expression", f"{p[1]}(expression)")

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = ('number', p[1])
    add_derivation_step("expression", "NUMBER")

def p_term(p):
    '''term : factor'''
    p[0] = p[1]
    add_derivation_step("term", "factor")

def p_factor(p):
    '''factor : NUMBER
              | LPAREN expression RPAREN
              | factor POWER factor
              | factor FACTORIAL'''
    if len(p) == 2:  # Single number
        p[0] = ('number', p[1])
        add_derivation_step("factor", "NUMBER")
    elif len(p) == 4 and p[2] == '^':  # Power operator
        p[0] = ('power', p[1], p[3])
        add_derivation_step("factor", "factor ^ factor")
    elif len(p) == 3 and p[2] == '!':  # Factorial
        p[0] = ('factorial', p[1])
        add_derivation_step("factor", "factor !")

# Error handling
def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}' (line {p.lineno})")
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

# 3. AST Evaluation
def evaluate_ast(ast):
    if ast[0] == 'number':
        return ast[1]
    elif ast[0] == 'binop':
        left = evaluate_ast(ast[2])
        right = evaluate_ast(ast[3])
        if ast[1] == '+':
            return left + right
        elif ast[1] == '-':
            return left - right
        elif ast[1] == '*':
            return left * right
        elif ast[1] == '/':
            return left / right
        elif ast[1] == '^':
            return left ** right
    elif ast[0] == 'factorial':
        return math.factorial(int(evaluate_ast(ast[1])))
    elif ast[0] == 'function':
        arg = evaluate_ast(ast[2])
        if ast[1] == 'sin':
            return math.sin(math.radians(arg))
        elif ast[1] == 'cos':
            return math.cos(math.radians(arg))

# Input and Execution
if __name__ == "__main__":
    expr = input("Enter a mathematical expression: ")
    lexer.input(expr)
    print("\nTokens:")
    for token in lexer:
        print(token)
    print("\nSyntax Analysis (Parse Tree):")
    parse_tree = parser.parse(expr)
    print(parse_tree)
    if parse_tree:
        print("\nAST Evaluation:")
        result = evaluate_ast(parse_tree)
        print(f"Result: {result}")
    print("\nDerivation Steps:")
    for step in derivation_steps:
        print(step)
