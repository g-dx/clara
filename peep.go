package main

// Very simple peep hole optimiser which collapses pairs of instructions together
type peep struct {
	w   asmWriter
	i   inst
	ops []operand
}

func NewOptimiser(w asmWriter) asmWriter {
	 return &peep{ w : w }
}

func (p *peep) ins(i inst, ops ...operand) {

	// Remove unnecessary "intermediate" register for mov, ensuring we don't attempt mem -> mem
	if p.i == movq && i == movq && regMatches(p.ops[1], ops[0]) && !(mem(p.ops[0]) && mem(ops[1])) {
		p.ops = []operand { p.ops[0], ops[1] }
		return
	}

	// Remove unnecessary "intermediate" register for push
	if p.i == movq && i == pushq && regMatches(p.ops[1], ops[0]) {
		p.i = i
		p.ops = []operand { p.ops[0] }
		return
	}

	// Flush cached (if any) & cache current instruction
	p.write()
	p.i = i
	p.ops = ops
}

func (p *peep) write() {
	if p.i > 0 {
		p.w.ins(p.i, p.ops...)
		p.i = 0
		p.ops = nil
	}
}

func regMatches(o1, o2 operand) bool {
	r1, ok1 := o1.(reg)
	r2, ok2 := o2.(reg)
	return ok1 && ok2 && r1 == r2
}

func mem(o operand) bool {
	_, ok := o.(memOp)
	return ok
}

func (p *peep) tab(s ...string)                          { p.write(); p.w.tab(s...) }
func (p *peep) spacer()                                  { p.write(); p.w.spacer() }
func (p *peep) stringLit(s string) operand               { p.write(); return p.w.stringLit(s) }
func (p *peep) label(s string)                           { p.write(); p.w.label(s) }
func (p *peep) newLabel(s string) string                 { p.write(); return p.w.newLabel(s) }
func (p *peep) raw(s string)                             { p.write(); p.w.raw(s) }
func (p *peep) addr(sym operand)                         { p.write(); p.w.addr(sym) }
func (p *peep) fnStart(name string)                      { p.write(); p.w.fnStart(name) }
func (p *peep) fnEnd()                                   { p.write() }
func (p *peep) roSymbol(name string, f func(w asmWriter)) operand { p.write(); return p.w.roSymbol(name, f) }
func (p *peep) gcMap(name string, offsets []int) labelOp { p.write(); return p.w.gcMap(name, offsets) }
func (p *peep) flush()                                   { p.w.flush() }