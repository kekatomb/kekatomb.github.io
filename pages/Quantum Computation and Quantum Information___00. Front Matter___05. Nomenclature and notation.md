- **note description**
  type:: #zettelkasten
  state:: #zettelkasten/literature
- ## Linear algebra and quantum mechanics
	- **Note:** For presentation purposes, all vector spaces are assumed to be finite dimensional.
	- ### Operators
		- A *positive* operator: 
		  $\forall A. \forall |\psi\rangle. \langle\psi| A|\psi\rangle\gt 0$
		- A *positive definite* operator:
		  $\forall A. \forall |\psi\rangle\ne 0. \langle\psi|A|\psi\rangle\gt 0$
	- ### Notation
		- $U$ denotes *unitary operators*/*matrices*.
		- $H$ denotes the *Hadamard quantum logic gate* and sometimes the *Hamiltonian* for a quantum system.
		- Vectors written in the format $(1,2)$ represent a column vector.
	- ### Qubits
		- $|0\rangle\equiv (1,0)$
		- $|1\rangle\equiv(0,1)$
	- ### Pauli Sigma $z$ Matrix
		- $\sigma_z|0\rangle=|0\rangle$
			- Note: here we're indexing the first column.
		- $\sigma_z|1\rangle=-|1\rangle$
			- Note: here we're indexing the second column.
		- $\sigma_0$ is defined as the $2\times2$ identity matrix.
		- $\langle \sigma_0,\sigma_x, \sigma_y, \sigma_z\rangle=\langle \sigma_1,\sigma_2,\sigma_3\rangle=\langle I,X,Y,Z\rangle$
			- Note: here I'm using angle brackets to denote a set.
			- Note: the subscripts are equivalent.
- ## Information theory and probability
	- Note: Logarithms are assumed to be taken to base two, thus $\log x$ denotes logarithms to base 2.
	- ### Definitions
		- *Probability distributions*:
		  $p_x\in\mathcal{P}_\text{fin}(\mathbb{R}). p_x\ge 0\land \sum_x p_x=1$
		- *Relative entropy* of a positive operator A with respect to a positive operator $B$:
		  $S(A||B)\equiv\text{Tr}\space A\log A-\text{Tr}\space A\log B$
- ## Miscellanea
	- ### Notation
		- $\oplus$ denotes modulo two addition.
	- ### Pronunciation
		- 'z' is pronounced 'zed'.