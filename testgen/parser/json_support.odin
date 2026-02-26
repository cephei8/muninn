package testgen

import "core:encoding/json"
import "core:odin/ast"
import "core:odin/tokenizer"

attribute_to_json :: proc(a: ^ast.Attribute) -> json.Value {
	m := make(json.Object)
	p := pos_to_json(a.pos)
	e := pos_to_json(a.end)
	m["node"] = json.String("Attribute")
	m["pos"] = p
	m["end"] = e
	m["exprs"] = exprs_to_json(a.elems)
	return m
}

attributes_to_json :: proc(attrs: [dynamic]^ast.Attribute) -> json.Value {
	arr := make(json.Array, 0, len(attrs))
	for a in attrs {
		append(&arr, attribute_to_json(a))
	}
	return arr
}

field_to_json :: proc(f: ^ast.Field) -> json.Value {
	m := make(json.Object)
	p := pos_to_json(f.pos)
	e := pos_to_json(f.end)
	m["node"] = json.String("Field")
	m["pos"] = p
	m["end"] = e
	m["names"] = exprs_to_json(f.names)
	m["type"] = nullable_expr(f.type)
	m["default"] = nullable_expr(f.default_value)

	if f.tag.text != "" {
		m["tag"] = json.String(clone_string(f.tag.text))
	} else {
		m["tag"] = nil
	}

	m["flags"] = field_flags_to_json(f.flags)
	return m
}

field_list_to_json :: proc(fl: ^ast.Field_List) -> json.Value {
	m := make(json.Object)
	p := pos_to_json(fl.pos)
	e := pos_to_json(fl.end)
	m["node"] = json.String("FieldList")
	m["pos"] = p
	m["end"] = e

	arr := make(json.Array, 0, len(fl.list))
	for f in fl.list {
		append(&arr, field_to_json(f))
	}
	m["fields"] = arr
	return m
}

bit_field_field_to_json :: proc(f: ^ast.Bit_Field_Field) -> json.Value {
	m := make(json.Object)
	p := pos_to_json(f.pos)
	e := pos_to_json(f.end)
	m["node"] = json.String("BitFieldField")
	m["pos"] = p
	m["end"] = e
	// Odin parser bug: parse_ident() produces Ident{name="_"} for keywords as bit_field field names
	m["name"] = expr_to_json(f.name)
	m["type"] = expr_to_json(f.type)
	m["size"] = expr_to_json(f.bit_size)
	return m
}

token_to_binop :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Add:         return "+"
	case .Sub:         return "-"
	case .Mul:         return "*"
	case .Quo:         return "/"
	case .Mod:         return "%"
	case .Mod_Mod:     return "%%"
	case .And:         return "&"
	case .Or:          return "|"
	case .Xor:         return "~"
	case .And_Not:     return "&~"
	case .Shl:         return "<<"
	case .Shr:         return ">>"
	case .Cmp_And:     return "&&"
	case .Cmp_Or:      return "||"
	case .Cmp_Eq:      return "=="
	case .Not_Eq:      return "!="
	case .Lt:          return "<"
	case .Gt:          return ">"
	case .Lt_Eq:       return "<="
	case .Gt_Eq:       return ">="
	case .In:          return "in"
	case .Not_In:      return "not_in"
	case .Range_Half:  return "..<"
	case .Range_Full:  return "..="
	case:              return "?"
	}
}

token_to_unaryop :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Add:     return "+"
	case .Sub:     return "-"
	case .Not:     return "!"
	case .Xor:     return "~"
	case .And:     return "&"
	case .Pointer: return "^"
	case .Dollar:  return "$"
	case:          return "?"
	}
}

token_to_assignop :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Eq:          return "="
	case .Add_Eq:      return "+="
	case .Sub_Eq:      return "-="
	case .Mul_Eq:      return "*="
	case .Quo_Eq:      return "/="
	case .Mod_Eq:      return "%="
	case .Mod_Mod_Eq:  return "%%="
	case .And_Eq:      return "&="
	case .Or_Eq:       return "|="
	case .Xor_Eq:      return "~="
	case .And_Not_Eq:  return "&~="
	case .Shl_Eq:      return "<<="
	case .Shr_Eq:      return ">>="
	case .Cmp_And_Eq:  return "&&="
	case .Cmp_Or_Eq:   return "||="
	case:              return "?"
	}
}

branch_kind_to_string :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Break:       return "break"
	case .Continue:    return "continue"
	case .Fallthrough: return "fallthrough"
	case .Or_Break:    return "break"
	case .Or_Continue: return "continue"
	case:              return "?"
	}
}

lit_kind_to_string :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Integer: return "integer"
	case .Float:   return "float"
	case .Imag:    return "imaginary"
	case .Rune:    return "rune"
	case .String:  return "string"
	case:          return "?"
	}
}

cast_kind_to_string :: proc(kind: tokenizer.Token_Kind) -> string {
	#partial switch kind {
	case .Cast:      return "cast"
	case .Transmute: return "transmute"
	case:            return "?"
	}
}

calling_convention_to_json :: proc(cc: ast.Proc_Calling_Convention) -> json.Value {
	s, ok := cc.(string)
	if !ok {
		return nil
	}
	stripped := s
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		stripped = s[1:len(s)-1]
	}
	return json.String(clone_string(stripped))
}

field_flags_to_json :: proc(flags: ast.Field_Flags) -> json.Value {
	arr := make(json.Array, 0, 4)
	if .Using in flags {
		append(&arr, json.Value(json.String("using")))
	}
	if .Any_Int in flags {
		append(&arr, json.Value(json.String("any_int")))
	}
	if .C_Vararg in flags {
		append(&arr, json.Value(json.String("c_vararg")))
	}
	if .No_Alias in flags {
		append(&arr, json.Value(json.String("no_alias")))
	}
	if .Subtype in flags {
		append(&arr, json.Value(json.String("subtype")))
	}
	return arr
}

struct_flags_to_json :: proc(n: ^ast.Struct_Type) -> json.Value {
	arr := make(json.Array, 0, 2)
	if n.is_packed {
		append(&arr, json.Value(json.String("packed")))
	}
	if n.is_raw_union {
		append(&arr, json.Value(json.String("raw_union")))
	}
	return arr
}

union_flags_to_json :: proc(n: ^ast.Union_Type) -> json.Value {
	arr := make(json.Array, 0, 2)
	#partial switch n.kind {
	case .no_nil:
		append(&arr, json.Value(json.String("no_nil")))
	case .shared_nil:
		append(&arr, json.Value(json.String("shared_nil")))
	}
	return arr
}

clone_string :: proc(s: string) -> string {
	if len(s) == 0 {
		return ""
	}
	buf := make([]u8, len(s))
	copy(buf, s)
	return string(buf)
}
