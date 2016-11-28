#!/bin/python
import subprocess
import os.path
import sys
import glob

test_files = os.path.join("lisp_tests", "*.zlt")

binary = "./worker"
test_separator = ";;;;TEST;;;;"
result_separator = ";=>"

def main():
    print("========")
    count_total = 0
    count_failed = 0
    if len(sys.argv) > 1:
        files = sys.argv[1:]
    else:
        files = glob.glob(test_files)
    for file in files:
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
    with open(file) as f:
        current_linenumber = 0
        linenumber = 0
        tests = []
        text = []
        ignore = False
        for line in f.readlines():
            linenumber += 1
            if line.startswith(test_separator):
                if not ignore:
                    tests.append((current_linenumber, "".join(text)))
                ignore = False
                text = []
                current_linenumber = linenumber
            else:
                if ";ignore;" in line:
                    ignore = True
                if line.startswith(";"):
                    continue
                text.append(line)
        if not ignore:
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
        print("ERROR, redundant line in test ({}:{}):\n{}".format(file, linenumber, code))
        return False

    return lisp(file, linenumber, statements, expected)


def lisp(file, linenumber, statements, expected_values):
    program = [binary, "-t"]
    for statement in statements:
        program.append("-e")
        program.append(statement)

    result = subprocess.run(program, universal_newlines=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        print("TEST-failed: {}:{}: program terminated unexpectedly:\n{}".format(file, linenumber, result.stderr))
        return False
    output = result.stdout

    output_list = output.strip().split("\n")
    if len(output_list) != len(expected_values):
        print("TEST-failed: {}:{}: \t\toutput length is different than expected".format(file, linenumber))
        print("Got:", output_list)
        print("Expected:", expected_values)
        return False

    for output, expected in zip(output_list, expected_values):
        if expected.startswith("error") and output.startswith("error"):
            continue
        if output.strip() == expected.strip():
            continue
        else:
            print(result.stderr)
            text = "\texpected {} got {}".format(expected, output)
            print("TEST-failed: {}:{}: \t\t{}".format(file, linenumber, text))
            return False
    return True

if __name__ == "__main__":
    sys.exit(main())
