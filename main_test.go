package main

import (
	"testing"
	"os"
	"os/exec"
	"strings"
	"fmt"
	"path/filepath"
	"log"
	"io/ioutil"
	"regexp"
	"bytes"
)

var regex = regexp.MustCompile("^.*?//\\sEXPECT:\\s(.*)$")

func TestE2E(t *testing.T) {
	files, err := filepath.Glob("./tests/*.clara")
	if err != nil {
		log.Fatal(err)
	}

	// Process each test case
	for _, f := range files {
		t.Run(filepath.Base(f), func(t *testing.T) {
			// Read file
			bytes, err := ioutil.ReadFile(f)
			if err != nil {
				t.Fatal(err)
			}
			test := string(bytes)
			steps := strings.Split(test, "\n")

			// Gather all expectations
			var expects []string
			for _, step := range steps {
				if strings.HasPrefix(step, "//") { // Skip commented out lines
					continue
				}
				match := regex.FindStringSubmatch(step)
				if match != nil {
					expects = append(expects, match[1]) // First group hold expectation
				}
			}

			// Compile test, execute test & parse output
			output := CompileAndRun(f, t)
			lines := strings.Split(output, "\n")
			lines = lines[:len(lines)-1] // Trim empty final line

			// Match output against expectations
			pos := 0
			for _, expect := range expects {
				if pos < len(lines) {
					if expect != lines[pos] {
						t.Errorf("- %v, expected: '%v', got: '%v'\n", f, expect, lines[pos])
					}
				} else {
					t.Errorf(" - %v, expected: '%v', got: <nothing>\n", f, expect)
				}
				pos += 1
			}
			if pos < len(lines) {
				t.Errorf(" - %v, expected: <nothing>, got: '%v'\n", f, strings.Join(lines[pos:], "\n"))
			}
		})
	}
}

func CompileAndRun(progPath string, t *testing.T) string {

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

	// Execute binary
	cmd := exec.Command(binary)
	cmd.Stderr = os.Stderr
	outBytes, err := cmd.Output()
	out := string(outBytes)
	if err != nil {
		t.Log(out)
		t.Fatalf("Execution failure: %v\n", err)
	}
	return out
}
