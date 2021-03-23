rm -rf testcases/script_Testcases
mkdir -p testcases/script_Testcases
mkdir -p testcases/script_Testcases/q1
mkdir -p testcases/script_Testcases/q2
mkdir -p testcases/script_Testcases/q3

rm -rf testcases/script_Outputs
mkdir -p testcases/script_Outputs
mkdir -p testcases/script_Outputs/q1
mkdir -p testcases/script_Outputs/q2
mkdir -p testcases/script_Outputs/q3


run_testcases () {
	for i in $(seq 1 ${2});
	do
		cat $3/$1.scm >> testcases/script_Testcases/$1/$i.scm
		echo "" >> testcases/script_Testcases/$1/$i.scm
		cat testcases/Testcases/$1/$i.txt >> testcases/script_Testcases/$1/$i.scm
		raco exe testcases/script_Testcases/$1/$i.scm
		./testcases/script_Testcases/$1/$i.exe > ./testcases/script_Outputs/$1/$i.txt
		sed -e '$a\' testcases/Outputs/$1/$i.txt > testcases/script_Outputs/$1/$i\_expected.txt
		if diff testcases/script_Outputs/$1/$i\_expected.txt testcases/script_Outputs/$1/$i.txt; then
			echo "✅ "$1"/"$i".txt"
		else
			echo "❌ "$1"/"$i".txt"
		fi
	done
}

run_testcases q1 15 b18080-pa1
run_testcases q2 15 b18080-pa1
run_testcases q3 20 b18080-pa1
