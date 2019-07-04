import os
 
def plot_qvl(cb,cl,ft):
	f=open("draw.dot","w")
	f.write("digraph G {\n  rankdir=LR\n  splines=line\n  size=\"12,12\";\n  \n  subgraph cluster_00\n  {\n    color=white;\n    node [style=solid,color=blue4, shape=point];")
	s=""
	for i in xrange(0,len(ft)):
		s=s+"p"+(str)(i)+" "
	f.write(s+"}\n")
	f.write("\n        \n  subgraph cluster_0\n  {\n    color=white;\n    node [style=solid,color=blue4, shape=circle];")
	s=""
	for i in xrange(0,len(ft)):
		s=s+"x"+(str)(i)+"[label=<"+ft[i]+">] "
	f.write(s+"\n")
	f.write("label = \"inputs\";\n  }\n  subgraph cluster_1\n  {\n    color=white;\n    node [style=solid,color=red2, shape=circle];")
	s=""
	for i in xrange(0,len(cl)):
		s=s+"v"+(str)(i)+"[label=<"+cl[i]+">] "
	f.write(s+"\n")
	f.write("label = \"LVQ\";\n  }\n  subgraph cluster_2\n  {\n    color=white;\n    node [style=solid,color=red2, shape=point];")
	s=""
	for i in xrange(0,len(cl)):
		s=s+"q"+(str)(i)+" "
	f.write(s+"\n}\n")
	s=""
	for i in xrange(0,len(ft)):
		s=s+"p"+(str)(i)+"->"+"x"+(str)(i)+"\n"
	f.write(s)
	s=""
	for i in xrange(0,len(cb[0])):
		for j in xrange(0,len(cb)):
			s=s+"x"+(str)(i)+"->"+"v"+(str)(j)+"[label=<"+(str)(cb[j][i])+">]\n"
	f.write(s)
	s=""
	for i in xrange(0,len(cl)):
		s=s+"v"+(str)(i)+"->"+"q"+(str)(i)+"\n"
	f.write(s+"}")
	f.close()
	os.system("dot -Tpng draw.dot -o lvq_p.png")
