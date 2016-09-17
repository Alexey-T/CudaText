// SYNTAX TEST "Packages/Pascal/Pascal.sublime-syntax"

  // double slash comment
// <- punctuation.whitespace.comment.leading.pascal
  // <- punctuation.definition.comment.pascal
  // ^^^^^^^^^^^^^^^^^^^^ comment.line.double-slash.pascal.two

  -- double dash comment
// <- punctuation.whitespace.comment.leading.pascal
  // <- punctuation.definition.comment.pascal
  // ^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.pascal.one

// comment
procedure foo;
// ^ meta.function.pascal
begin
	// comment
end;
// <- keyword.control.pascal

-- comment
procedure bar;
// ^ meta.function.pascal
begin
	-- comment
end;
// <- keyword.control.pascal
