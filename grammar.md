# Grammar

expr: term ((PLUS/MINUS)term)*
term: factor ((MUL/DIV)factor)*
factor: INTEGER