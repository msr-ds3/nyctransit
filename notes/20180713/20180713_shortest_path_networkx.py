import networkx as nx
def shortest_paths(file_path, num_paths):
    #initialize graph
    subway_graph = nx.DiGraph()
    file = open(file_path)
    
    #read in edges from file
    for line in file:
        line_num, from_id, to_id, w = line.split(",")
        subway_graph.add_edge(from_id[1:-1], to_id[1:-1], weight = float(w[:-1]))
    
    #compute shortest paths
    paths = nx.shortest_simple_paths(subway_graph, "R19", "104", "weight")
    for i in range(num_paths):
        print(next(paths))

if __name__ == "__main__":
    #parameters are file name and number of paths
    shortest_paths("../../data/edgelist.csv", 3)




