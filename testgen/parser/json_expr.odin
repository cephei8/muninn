package testgen

import "core:encoding/json"
import "core:odin/ast"
import "core:odin/tokenizer"

expr_to_json :: proc(e: ^ast.Expr) -> json.Value {
	if e == nil {
		return nil
	}

	#partial switch n in e.derived_expr {
	case ^ast.Ident:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("Ident")
		m["pos"] = p
		m["end"] = end
		m["name"] = json.String(clone_string(n.name))
		return m

	case ^ast.Basic_Lit:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BasicLit")
		m["pos"] = p
		m["end"] = end
		m["kind"] = json.String(lit_kind_to_string(n.tok.kind))
		m["value"] = json.String(clone_string(n.tok.text))
		return m

	case ^ast.Unary_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("UnaryExpr")
		m["pos"] = p
		m["end"] = end
		m["op"] = json.String(token_to_unaryop(n.op.kind))
		m["expr"] = nullable_expr(n.expr)
		return m

	case ^ast.Binary_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BinaryExpr")
		m["pos"] = p
		m["end"] = end
		m["lhs"] = expr_to_json(n.left)
		m["op"] = json.String(token_to_binop(n.op.kind))
		m["rhs"] = expr_to_json(n.right)
		return m

	case ^ast.Paren_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("ParenExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Call_Expr:
		return call_expr_to_json(n)

	case ^ast.Index_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("IndexExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		m["index"] = expr_to_json(n.index)
		return m

	case ^ast.Slice_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("SliceExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		m["low"] = nullable_expr(n.low)
		m["high"] = nullable_expr(n.high)
		return m

	case ^ast.Selector_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("SelectorExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		json_sel := expr_to_json(cast(^ast.Expr)n.field)
		m["sel"] = json_sel
		return m

	case ^ast.Implicit_Selector_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("ImplicitSelectorExpr")
		m["pos"] = p
		m["end"] = end
		m["sel"] = expr_to_json(cast(^ast.Expr)n.field)
		return m

	case ^ast.Selector_Call_Expr:
		return call_expr_to_json(n.call)

	case ^ast.Deref_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("DerefExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Comp_Lit:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("CompLit")
		m["pos"] = p
		m["end"] = end
		m["type"] = nullable_expr(n.type)
		m["elems"] = exprs_to_json(n.elems)
		return m

	case ^ast.Proc_Lit:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("ProcLit")
		m["pos"] = p
		m["end"] = end
		m["type"] = proc_type_to_json(n.type)
		m["body"] = nullable_stmt(n.body)
		return m

	case ^ast.Proc_Group:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("ProcGroup")
		m["pos"] = p
		m["end"] = end
		m["exprs"] = exprs_to_json(n.args)
		return m

	case ^ast.Ternary_If_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TernaryIfExpr")
		m["pos"] = p
		m["end"] = end
		m["cond"] = expr_to_json(n.cond)
		m["then"] = expr_to_json(n.x)
		m["else"] = expr_to_json(n.y)
		return m

	case ^ast.Ternary_When_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TernaryWhenExpr")
		m["pos"] = p
		m["end"] = end
		m["cond"] = expr_to_json(n.cond)
		m["then"] = expr_to_json(n.x)
		m["else"] = expr_to_json(n.y)
		return m

	case ^ast.Or_Else_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("OrElseExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.x)
		m["or_else"] = expr_to_json(n.y)
		return m

	case ^ast.Or_Return_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("OrReturnExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Or_Branch_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("OrBranchExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		m["kind"] = json.String(branch_kind_to_string(n.token.kind))
		m["label"] = nullable_expr(n.label)
		return m

	case ^ast.Type_Assertion:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TypeAssertion")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		m["type"] = expr_to_json(n.type)
		return m

	case ^ast.Type_Cast:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TypeCast")
		m["pos"] = p
		m["end"] = end
		m["kind"] = json.String(cast_kind_to_string(n.tok.kind))
		m["type"] = expr_to_json(n.type)
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Auto_Cast:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("AutoCast")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Ellipsis:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("Ellipsis")
		m["pos"] = p
		m["end"] = end
		m["expr"] = nullable_expr(n.expr)
		return m

	case ^ast.Field_Value:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("FieldValue")
		m["pos"] = p
		m["end"] = end
		m["field"] = expr_to_json(n.field)
		m["value"] = expr_to_json(n.value)
		return m

	case ^ast.Undef:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("Undef")
		m["pos"] = p
		m["end"] = end
		return m

	case ^ast.Implicit:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("Implicit")
		m["pos"] = p
		m["end"] = end
		return m

	case ^ast.Basic_Directive:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BasicDirective")
		m["pos"] = p
		m["end"] = end
		m["name"] = json.String(clone_string(n.name))
		m["expr"] = nil
		return m

	case ^ast.Tag_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TagExpr")
		m["pos"] = p
		m["end"] = end
		m["tag"] = json.String(clone_string(n.name))
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Pointer_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("PointerType")
		m["pos"] = p
		m["end"] = end
		m["elem"] = expr_to_json(n.elem)
		return m

	case ^ast.Multi_Pointer_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("MultiPointerType")
		m["pos"] = p
		m["end"] = end
		m["elem"] = expr_to_json(n.elem)
		return m

	case ^ast.Array_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("ArrayType")
		m["pos"] = p
		m["end"] = end
		m["len"] = nullable_expr(n.len)
		m["elem"] = expr_to_json(n.elem)
		return m

	case ^ast.Dynamic_Array_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("DynamicArrayType")
		m["pos"] = p
		m["end"] = end
		m["elem"] = expr_to_json(n.elem)
		return m

	case ^ast.Map_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("MapType")
		m["pos"] = p
		m["end"] = end
		m["key"] = expr_to_json(n.key)
		m["value"] = expr_to_json(n.value)
		return m

	case ^ast.Struct_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("StructType")
		m["pos"] = p
		m["end"] = end
		m["fields"] = field_list_to_json(n.fields)
		m["align"] = nullable_expr(n.align)
		m["flags"] = struct_flags_to_json(n)
		return m

	case ^ast.Union_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("UnionType")
		m["pos"] = p
		m["end"] = end
		m["variants"] = exprs_to_json(n.variants)
		m["params"] = nullable_field_list(n.poly_params)
		m["flags"] = union_flags_to_json(n)
		return m

	case ^ast.Enum_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("EnumType")
		m["pos"] = p
		m["end"] = end
		m["backing_type"] = nullable_expr(n.base_type)
		m["fields"] = exprs_to_json(n.fields)
		return m

	case ^ast.Bit_Set_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BitSetType")
		m["pos"] = p
		m["end"] = end
		m["elem"] = expr_to_json(n.elem)
		m["underlying"] = nullable_expr(n.underlying)
		return m

	case ^ast.Bit_Field_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BitFieldType")
		m["pos"] = p
		m["end"] = end
		m["backing_type"] = nullable_expr(n.backing_type)
		fields_arr := make(json.Array, 0, len(n.fields))
		for f in n.fields {
			append(&fields_arr, bit_field_field_to_json(f))
		}
		m["fields"] = fields_arr
		return m

	case ^ast.Proc_Type:
		return proc_type_to_json(n)

	case ^ast.Matrix_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("MatrixType")
		m["pos"] = p
		m["end"] = end
		m["rows"] = expr_to_json(n.row_count)
		m["cols"] = expr_to_json(n.column_count)
		m["elem"] = expr_to_json(n.elem)
		return m

	case ^ast.Distinct_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("DistinctType")
		m["pos"] = p
		m["end"] = end
		m["type"] = expr_to_json(n.type)
		return m

	case ^ast.Poly_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("PolyType")
		m["pos"] = p
		m["end"] = end
		m["name"] = expr_to_json(cast(^ast.Expr)n.type)
		m["specialization"] = nullable_expr(n.specialization)
		return m

	case ^ast.Typeid_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("TypeidType")
		m["pos"] = p
		m["end"] = end
		m["specialization"] = nullable_expr(n.specialization)
		return m

	case ^ast.Helper_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("HelperType")
		m["pos"] = p
		m["end"] = end
		m["type"] = expr_to_json(n.type)
		return m

	case ^ast.Relative_Type:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("RelativeType")
		m["pos"] = p
		m["end"] = end
		m["tag"] = expr_to_json(n.tag)
		m["type"] = expr_to_json(n.type)
		return m

	case ^ast.Inline_Asm_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("InlineAsmExpr")
		m["pos"] = p
		m["end"] = end
		m["params"] = exprs_to_json(n.param_types)
		m["return"] = expr_to_json(n.return_type)
		return m

	case ^ast.Matrix_Index_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("MatrixIndexExpr")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		m["row"] = expr_to_json(n.row_index)
		m["col"] = expr_to_json(n.column_index)
		return m

	case ^ast.Bad_Expr:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.expr_base)
		m["node"] = json.String("BadExpr")
		m["pos"] = p
		m["end"] = end
		return m

	case:
		m := make(json.Object)
		m["node"] = json.String("UnknownExpr")
		return m
	}
}

call_expr_to_json :: proc(n: ^ast.Call_Expr) -> json.Value {
	m := make(json.Object)
	p, end := node_pos_end(&n.node.expr_base)
	m["node"] = json.String("CallExpr")
	m["pos"] = p
	m["end"] = end
	m["func"] = expr_to_json(n.expr)
	m["args"] = exprs_to_json(n.args)
	has_ellipsis := n.ellipsis.pos.offset != 0
	m["ellipsis"] = json.Boolean(has_ellipsis)
	return m
}

proc_type_to_json :: proc(n: ^ast.Proc_Type) -> json.Value {
	m := make(json.Object)
	p, end := node_pos_end(&n.node.expr_base)
	m["node"] = json.String("ProcType")
	m["pos"] = p
	m["end"] = end
	m["params"] = field_list_to_json(n.params)
	m["results"] = nullable_field_list(n.results)
	m["calling_convention"] = calling_convention_to_json(n.calling_convention)
	return m
}
