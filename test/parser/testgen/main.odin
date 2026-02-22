package testgen

import "core:encoding/json"
import "core:fmt"
import "core:odin/ast"
import "core:odin/parser"
import "core:odin/tokenizer"
import "core:os"
import "core:strings"

main :: proc() {
	args := os.args
	if len(args) < 2 {
		fmt.eprintln("Usage: testgen <file>...")
		fmt.eprintln("       testgen generate <src_root> <golden_dir> <subdirs...>")
		os.exit(1)
	}

	if args[1] == "generate" {
		if len(args) < 5 {
			fmt.eprintln("Usage: testgen generate <src_root> <golden_dir> <subdirs...>")
			os.exit(1)
		}
		generate(args[2], args[3], args[4:])
	} else {
		for path in args[1:] {
			json_str, ok := parse_to_json(path)
			if !ok { continue }
			fmt.println(json_str)
		}
	}
}

parse_to_json :: proc(path: string) -> (string, bool) {
	data, ok := os.read_entire_file(path)
	if !ok {
		fmt.eprintfln("Error: could not read file: %s", path)
		return "", false
	}

	src := string(data)
	p := parser.default_parser()

	NO_POS :: tokenizer.Pos{}
	f := ast.new(ast.File, NO_POS, NO_POS)
	f.src = src
	f.fullpath = path

	// Odin parser bug: parse_file() rejects reserved package names ("builtin", "intrinsics").
	// We still emit JSON for these files (pkg_name is set, decls empty).
	parser.parse_file(&p, f)

	result := file_to_json(f)
	json_bytes, marshal_err := json.marshal(result, {pretty = true, use_spaces = true, spaces = 2, sort_maps_by_key = true})
	if marshal_err != nil {
		fmt.eprintfln("Error: could not marshal JSON for: %s", path)
		return "", false
	}
	return string(json_bytes), true
}

file_to_json :: proc(f: ^ast.File) -> json.Value {
	m := make(json.Object)
	p := pos_to_json(f.pos)
	e := pos_to_json(f.end)

	// Comments
	comments := make(json.Array, 0, len(f.comments))
	for cg in f.comments {
		append(&comments, comment_group_to_json(cg))
	}
	m["comments"] = comments

	m["decls"] = stmts_to_json(f.decls[:])

	// Docs
	if f.docs != nil {
		m["docs"] = comment_group_to_json(f.docs)
	} else {
		m["docs"] = nil
	}

	m["end"] = e
	m["node"] = json.String("File")
	m["package"] = json.String(clone_string(f.pkg_name))
	m["pos"] = p
	return m
}

generate :: proc(src_root, golden_dir: string, subdirs: []string) {
	remove_all(golden_dir)
	passed := 0
	skipped := 0
	for subdir in subdirs {
		dir := strings.concatenate({src_root, "/", subdir})
		walk(dir, src_root, golden_dir, &passed, &skipped)
	}
	fmt.eprintfln("%d generated, %d skipped", passed, skipped)
}

walk :: proc(dir, src_root, golden_dir: string, passed, skipped: ^int) {
	fd, err := os.open(dir)
	if err != os.ERROR_NONE { return }
	entries, _ := os.read_dir(fd, -1)
	os.close(fd)
	for entry in entries {
		if entry.is_dir {
			walk(entry.fullpath, src_root, golden_dir, passed, skipped)
		} else if strings.has_suffix(entry.name, ".odin") {
			generate_one(entry.fullpath, src_root, golden_dir, passed, skipped)
		}
	}
}

generate_one :: proc(odin_file, src_root, golden_dir: string, passed, skipped: ^int) {
	json_str, ok := parse_to_json(odin_file)
	if !ok || len(json_str) == 0 {
		skipped^ += 1
		return
	}

	rel := strings.trim_prefix(odin_file, src_root)
	rel = strings.trim_prefix(rel, "/")
	base := strings.trim_suffix(rel, ".odin")
	golden := strings.concatenate({golden_dir, "/", base, ".json"})

	make_dirs(dir_of(golden))

	content := strings.concatenate({json_str, "\n"})
	if os.write_entire_file(golden, transmute([]u8)content) {
		passed^ += 1
	} else {
		skipped^ += 1
	}
}

dir_of :: proc(path: string) -> string {
	i := strings.last_index_byte(path, '/')
	if i < 0 { return "." }
	return path[:i]
}

make_dirs :: proc(path: string) {
	if os.exists(path) { return }
	parent := dir_of(path)
	if parent != path && len(parent) > 0 {
		make_dirs(parent)
	}
	os.make_directory(path)
}

remove_all :: proc(path: string) {
	if !os.exists(path) { return }
	fd, err := os.open(path)
	if err != os.ERROR_NONE { return }
	entries, _ := os.read_dir(fd, -1)
	os.close(fd)
	for entry in entries {
		if entry.is_dir {
			remove_all(entry.fullpath)
		} else {
			os.remove(entry.fullpath)
		}
	}
	os.remove(path)
}
