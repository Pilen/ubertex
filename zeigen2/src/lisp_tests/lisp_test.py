#!/bin/python
import subprocess
import os.path
import sys

test_files_dir = "lisp_tests"
test_files =[
    "basic.zlt",
    "simple.zlt",
    "comparison.zlt",
    "defun.zlt",
    # "tests.zlt"
]

repl = "./repl"
test_separator = ";;;;TEST;;;;"
result_separator = ";=>"

def main():
    print("========")
    count_total = 0
    count_failed = 0
    for file in test_files:
        print("{}:".format(file))
        count, failed = test_file(file)
        count_total += count
        count_failed += failed
    if count_failed == 0:
        print("{}/{} lisp tests completed successfully.".format(count_total - count_failed, count_total))
        print("========")
        return 0

    else:
        print("----")
        print("TESTS FAILED!")
        print("{} tests out of {} failed".format(count_failed, count_total))
        print("========")
        return -1


def test_file(file):
    with open(os.path.join(test_files_dir, file)) as f:
        current_linenumber = 0
        linenumber = 0
        tests = []
        text = []
        for line in f.readlines():
            linenumber += 1
            if line.startswith(test_separator):
                tests.append((current_linenumber, "".join(text)))
                text = []
                current_linenumber = linenumber
            else:
                text.append(line)
        tests.append((current_linenumber, "".join(text)))
        # tests = f.read().split(test_separator)

        total = 0
        failed = 0
        for i, (linenumber, test_case) in enumerate(tests):
            if test_case.strip() == "":
                continue
            total += 1
            if not test(file, linenumber, test_case):
                failed += 1
        return (total, failed)


def test(file, linenumber, code):
    code = code.strip()
    statements = []
    expected = []
    parts = code.split(result_separator)
    statements.append(parts.pop(0));
    for item in parts:
        pair = item.split("\n", 1)
        expected.append(pair[0].strip())
        if (len(pair) > 1):
            statements.append(pair[1])

    if len(statements) != len(expected):
        print("ERROR, redundant line in test:\n{}".format(code))
        return False

    return lisp(file, linenumber, statements, expected)

def lisp(file, linenumber, statements, expected_values):
    program = [repl, "-t", "-l", "0"]
    for statement in statements:
        program.append("-e")
        program.append(statement)

    output = subprocess.check_output(program, universal_newlines=True)
    output_list = output.strip().split("\n")
    if len(output_list) != len(expected_values):
        print("TEST-failed: {}:{}: \t\toutput length is different than expected".format(file, linenumber))
        return False

    for output, expected in zip(output_list, expected_values):
        if expected.startswith("error") and output.startswith("error"):
            continue
        if output.strip() == expected.strip():
            continue
        else:
            text = "\texpected {} got {}".format(expected, output)
            print("TEST-failed: {}:{}: \t\t{}".format(file, linenumber, text))
            return False
    return True

if __name__ == "__main__":
    sys.exit(main())
