# Binary Decision Diagram (BDD) Mid Project

## Overview
This project involves implementing an automatic construction of a Binary Decision Diagram (BDD) to represent Boolean functions. The assignment requires creating a program in Erlang that:
1. Constructs and optimizes BDDs using Shannon's Expansion Theory.
2. Solves BDDs for given Boolean input assignments.
3. Analyzes performance based on different tree representations (using maps and records).

---

## Objectives
1. Implement a function to convert Boolean expressions to BDDs.
2. Apply reduction rules to optimize BDDs:
   - Merge identical sub-graphs.
   - Remove redundant nodes.
3. Solve BDDs for various input assignments.
4. Compare performance for trees using maps vs. records.
5. Provide statistical analysis and conclusions on performance.

---

## Key Functions

### **`exp_to_bdd/3`**
- **Description:** Converts a Boolean function to its BDD representation.
- **Inputs:**
  - `BoolFunc`: Boolean function to be represented.
  - `Ordering`: Optimization criteria (e.g., `tree_height`, `num_of_nodes`, `num_of_leafs`).
  - `DataStructureType`: Specifies the use of `map` or `record` for storage.
- **Output:** The optimized BDD tree.

---

### **`solve_bdd/2`**
- **Description:** Evaluates the Boolean function represented by the BDD for specific variable assignments.
- **Inputs:**
  - `BddTree`: The BDD tree structure.
  - `Assignments`: A list of variable assignments (e.g., `[{x1, true}, {x2, false}]`).
- **Output:** The result of the Boolean function for the given assignments.

---

### **`listOfLeaves/1`**
- **Description:** Returns a list of pointers to the leaf nodes in the BDD.

---

### **`reverseIteration/1`**
- **Description:** Traces the shortest path from a specified leaf node back to the root.
- **Input:** A pointer to a leaf node.
- **Output:** A list of nodes representing the shortest path to the root.

---

## Example Boolean Function
The Boolean function:

\[
f(x_1, x_2, x_3, x_4) = x_1 \cdot x_2 + x_3 \cdot (x_4 + x_2) + x_4 \cdot x_1
\]

---

## Experimental Results
### Comparison of Tree Representations
| Metric         | Records (Average) | Maps (Average) |
|----------------|--------------------|----------------|
| Tree Height    | 260.8             | 426.2          |
| Number of Nodes| 306.8             | 342.6          |
| Number of Leaves | 257.2           | 423.6          |

### Time Measurements
The time taken to convert the Boolean function to a BDD and solve it for various assignments was measured. The results show that **records** outperform **maps** in terms of memory usage and computation speed for this implementation.

---

## Conclusions
1. **Performance:** Using records for BDD representation is more efficient due to their lower overhead and better memory management compared to maps.
2. **Tree Optimization:** Optimizing BDDs by merging identical sub-graphs and removing redundant nodes significantly reduces computation time and memory usage.
3. **Future Work:** Explore parallel computing approaches to further optimize BDD construction and solving.

---

## Suggested Parallel Computing Approach
- **Divide and Conquer:** Split the Boolean function into smaller sub-functions, construct partial BDDs in parallel, and then merge them.
- **Task Parallelism:** Assign separate threads or processes to evaluate different parts of the BDD tree simultaneously.
- **Load Balancing:** Ensure even distribution of tasks across processors to avoid bottlenecks.

---

## Files Included
1. **Code Implementation:** `middleProj.erl`
2. **Project Report:** `middle project.pdf`
3. **Experimental Data:** `BDD_final 2023.pdf`
