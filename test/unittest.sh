run () {
	rm -rf ../results
	mkdir -p ../results/ProtoPan/
	netlogo-headless.sh --model $PWD/../code/B3GET.nlogo --setup-file unittest.xml --experiment dine_and_dash --threads 1
}

#run
#mv ../results ./results1
run
rm -rf ./results2
mv ../results ./results2