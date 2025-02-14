import os
import subprocess
import time

REPETITIONS = 50

exp_dir = os.path.join(os.path.dirname(__file__), "..", "exps", "multi-steps")
bw_exp = os.path.join(exp_dir, "blocks_world")
arch_exp = os.path.join(exp_dir, "arch")

kbs_lists = [
    (2, os.path.join(bw_exp, "1", "4o", "kb", "kb_ll.pl")),
        (4, os.path.join(bw_exp, "2", "4o", "kb", "kb_ll_correct.pl")),
        (8, os.path.join(bw_exp, "3", "4o", "kb", "kb_ll_correct_v1.pl")),
        (6, os.path.join(bw_exp, "4", "4o", "kb", "kb_ll_correct.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_5_6.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_5_11.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_5_16.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_5_21.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_10_11.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_10_16.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_10_21.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_15_16.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_15_21.pl")),
        (6, os.path.join(bw_exp, "5", "4o", "kb", "kb_ll_correct_20_21.pl")),
        (6, os.path.join(arch_exp, "1", "4o", "kb", "kb_ll_correct.pl")),
        (6, os.path.join(arch_exp, "2", "4o", "kb", "kb_ll_correct.pl"))
]

def test_python():
    for _, file in kbs_lists:
        assert os.path.exists(file), f"KB file not found: {file}"

    for len, kb_path in kbs_lists:
        for i in range(REPETITIONS):
            print(f"Running test {i+1}/{10} for {kb_path}")
            # subprocess.call(["pwd"])
            subprocess.call(["python3", "planner.py", "-t", "-l", str(len), "-i", kb_path])
            time.sleep(1)
            print("Done\n")

def test_prolog():
    for len, kb_path in kbs_lists:
        for i in range(REPETITIONS):
            print(f"Running test {i+1}/{1} for {kb_path}")
            prolog_dir = os.path.join(os.path.dirname(__file__), "..", "prolog_planner")
            script_path = os.path.join(prolog_dir, "execute.sh")
            p = subprocess.Popen([script_path, prolog_dir, kb_path, str(len)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            output, _ = p.communicate()

            import re 
            match = re.search(r"High-level plan generated in (?P<HL_time>.+)s", output)
            hl_time = float(match["HL_time"])
            match = re.search(r"Low-level plan generated in (?P<LL_time>.+)s", output)
            ll_time = float(match["LL_time"])
            match = re.search(r"Achievers extracted in (?P<Ach_time>.+)s", output)
            ach_time = float(match["Ach_time"])

            import csv 
            with open("results_prolog.csv", mode="a") as file:
                writer = csv.writer(file)
                writer.writerow([kb_path, hl_time, ll_time, ach_time])

            print("Done") 


def parse_results(csv_file):
    import csv
    data_dict = dict()
    with open(csv_file, mode="r") as file:
        csv_reader = csv.reader(file)
        for row in csv_reader:
            if row[0] in data_dict:
                data_dict[row[0]].append(float(row[1]))
            else:
                data_dict[row[0]] = [float(row[1])]

    print(data_dict)

    for key in data_dict:
        print(f"{key}: {sum(data_dict[key])/len(data_dict[key])} with std: {sum([(x - sum(data_dict[key])/len(data_dict[key]))**2 for x in data_dict[key]])/len(data_dict[key])}")


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        if sys.argv[1] == "1":
            test_python()
        elif sys.argv[1] == "2":
            test_prolog()
        

    # test_python()
    # test_prolog()
    # parse_results("results.csv")