package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

var regex = regexp.MustCompile("^.*?//\\sEXPECT:\\s(.*)$")

type expectation struct {
	val string
	line int
}

func TestPanic(t *testing.T) {

	files, err := filepath.Glob("./tests/panic/*.clara")
	if err != nil {
		log.Fatal(err)
	}

	// Process each test case
	for _, f := range files {
		t.Run(filepath.Base(f), func(t *testing.T) {
			f := f
			t.Parallel()
			expects := ParseExpectations(f, t)
			if len(expects) != 1 {
				t.Fatalf("Only one expectation allowed for panic tests - found %d\n", len(expects))
			}
			expect := expects[0]
			out := CompileAndRun(f, t, true)
			if !strings.Contains(out, expect.val) {
				t.Errorf("\n- ./%v:\n - contains: '%v'\n - got     : '%v'\n", f, expect.val, out)
			}
		})
	}
}

func TestE2E(t *testing.T) {
	files, err := filepath.Glob("./tests/*.clara")
	if err != nil {
		log.Fatal(err)
	}

	// Process each test case
	for _, f := range files {
		t.Run(filepath.Base(f), func(t *testing.T) {
			f := f
			t.Parallel()
			expects := ParseExpectations(f, t)

			// Compile test, execute test & parse output
			output := CompileAndRun(f, t, false)
			lines := strings.Split(output, "\n")
			lines = lines[:len(lines)-1] // Trim empty final line

			// Match output against expectations
			pos := 0
			var builder strings.Builder
			for _, expect := range expects {
				if pos < len(lines) {
					if expect.val != lines[pos] {
						builder.WriteString(fmt.Sprintf("- ./%v:%d:, expected: '%v', got: '%v'\n", f, expect.line, expect.val, lines[pos]))
					}
				} else {
					builder.WriteString(fmt.Sprintf("- ./%v:%d:, expected: '%v', got: <nothing>\n", f, expect.line, expect.val))
				}
				pos += 1
			}
			if pos < len(lines) {
				builder.WriteString(fmt.Sprintf(" - ./%v:, expected: <nothing>, got: ['%v']\n", f, strings.Join(lines[pos:], "', '")))
			}
			if builder.Len() > 0 {
				t.Errorf("\n%v\n", builder.String())
			}
		})
	}
}

func CompileAndRun(progPath string, t *testing.T, allowExecErr bool) string {
	defer func() {
		if r := recover(); r != nil {
			t.Fatalf("\nCompiler Crash: %s\n", progPath)
		}
	}()

	// Compile program
	binary, errs := Compile(
		options{}, // Defaults
		glob("./install/lib/*.clara"),
		progPath,
		glob("./install/init/*.c"),
		os.TempDir(),
		ioutil.Discard)

	if len(errs) > 0 {
		buf := bytes.NewBufferString("\n\nCompilation failure(s):\n")
		for _, err := range errs {
			buf.WriteString(fmt.Sprintf("\n%v", err))
		}
		buf.WriteString("\n────────────────────────────────────────────────────────────────────────────────────────")
		t.Fatalf(buf.String())
	}
	defer os.Remove(binary)

	// Always set var
	err := os.Setenv("CLARA_ENV_KEY", "CLARA_ENV_VAL")
	if err != nil {
		t.Fatalf("Execution failure: %v\n", err)
	}

	// Execute binary
	cmd := exec.Command(binary)
	outBytes, err := cmd.CombinedOutput()
	out := string(outBytes)
	if err != nil && !allowExecErr {
		t.Log(out)
		t.Fatalf("Execution failure: %v\n", err)
	}
	return out
}

func ParseExpectations(filename string, t *testing.T) []*expectation {
	// Read file
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		t.Fatal(err)
	}
	steps := strings.Split(string(content), "\n")

	// Gather all expectations
	var expects []*expectation
	for i, step := range steps {
		if strings.HasPrefix(step, "//") { // Skip commented out lines
			continue
		}
		match := regex.FindStringSubmatch(step)
		if match != nil {
			expects = append(expects, &expectation{val: match[1], line: i+1})
		}
	}
	return expects
}