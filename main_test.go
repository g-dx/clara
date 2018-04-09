package main

import (
	"testing"
	"os"
	"os/exec"
	"strings"
	"fmt"
)

func TestHelloWorld(t *testing.T) {

	CompileAndRun(`
    fn main() {
      printf("Hello world!\n")
    }`,
    "Hello world!\n", t)
}

func CompileAndRun(program string, expectedOut string, t *testing.T) {

	binary := Compile(
		options{}, // Defaults
		glob("./install/lib/*.clara"),
		strings.NewReader(program),
		fmt.Sprintf("/compiler/tests/%v", t.Name()),
		glob("./install/init/*.c"), os.TempDir())

	// Execute binary
	cmd := exec.Command(binary)
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	if err != nil {
		t.Errorf("Failed to run binary: %v\n", err)
	}
	actualOutput := string(out)
	if expectedOut != actualOutput {
		t.Errorf("Expected: '%v', Actual: '%v'\n", expectedOut, actualOutput)
	}
}
