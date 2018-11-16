package main

type Packages struct {
	pkgs map[string]*Package
	root *Node
}

func NewPackages(root *Node) *Packages {
	pkgs := &Packages{pkgs: make(map[string]*Package), root: root}

	// --------------------------------------------------------------
	// TEMP: All definition not part of an explicit package go here
	//       This helps with the package transition
	pkg := &Package{name: "ROOT", root: root}
	pkgs.pkgs[pkg.name] = pkg
	// --------------------------------------------------------------

	return pkgs
}

func (p *Packages) lookup(name string) *Package {
	if pkg, ok := p.pkgs[name]; ok {
		return pkg
	}

	// Create AST & link to root
	n := &Node{op: opPackage, symtab: p.root.symtab.Child()}
	p.root.Add(n)

	pkg := &Package{}
	pkg.name = name
	pkg.root = n
	p.pkgs[name] = pkg
	return pkg
}

type Package struct {
	name    string
	root    *Node
	imports []*Package
}

func (p *Package) add(i *Package) {
	p.imports = append(p.imports, i)
}
