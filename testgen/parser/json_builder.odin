package testgen

import "core:encoding/json"
import "core:odin/ast"
import "core:odin/tokenizer"

pos_to_json :: proc(p: tokenizer.Pos) -> json.Value {
	m := make(json.Object)
	m["col"] = json.Integer(p.column)
	m["line"] = json.Integer(p.line)
	m["offset"] = json.Integer(p.offset)
	return m
}

node_pos_end :: proc(n: ^ast.Node) -> (json.Value, json.Value) {
	return pos_to_json(n.pos), pos_to_json(n.end)
}

nullable_expr :: proc(e: ^ast.Expr) -> json.Value {
	if e == nil {
		return nil
	}
	return expr_to_json(e)
}

nullable_stmt :: proc(s: ^ast.Stmt) -> json.Value {
	if s == nil {
		return nil
	}
	return stmt_to_json(s)
}

nullable_field_list :: proc(fl: ^ast.Field_List) -> json.Value {
	if fl == nil {
		return nil
	}
	return field_list_to_json(fl)
}

comment_group_to_json :: proc(cg: ^ast.Comment_Group) -> json.Value {
	m := make(json.Object)
	m["end"] = pos_to_json(cg.end)
	list := make(json.Array, 0, len(cg.list))
	for tok in cg.list {
		cm := make(json.Object)
		cm["pos"] = pos_to_json(tok.pos)
		cm["text"] = json.String(clone_string(tok.text))
		append(&list, cm)
	}
	m["list"] = list
	m["pos"] = pos_to_json(cg.pos)
	return m
}

exprs_to_json :: proc(es: []^ast.Expr) -> json.Value {
	arr := make(json.Array, 0, len(es))
	for e in es {
		append(&arr, expr_to_json(e))
	}
	return arr
}

stmts_to_json :: proc(ss: []^ast.Stmt) -> json.Value {
	arr := make(json.Array, 0, len(ss))
	for s in ss {
		// Skip Empty_Stmt nodes - Haskell parser doesn't produce them
		if s.derived_stmt == nil {
			continue
		}
		_, is_empty := s.derived_stmt.(^ast.Empty_Stmt)
		if is_empty {
			continue
		}
		append(&arr, stmt_to_json(s))
	}
	return arr
}
