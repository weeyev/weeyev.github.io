---
title: everything quantum
date: April 19, 2025
description: A brief overview for anyone who has just heard of quantum computing and can't figure out where to start.
readtime: 6 min read
tags: quantum, tutorial
---


## Purpose

This is a brief overview for anyone who has just heard of quantum computing and can't figure out where to start it. It's overwhelming at first but I think this should be a good start to start tinkering around on your own. A number of other resources that I found personally helpful will be linked in the end.

For any issues/ information that is quoted wrong, dm me on [x dot com]

## Quantum Computing and Qiskit

Examples are the best way to learn anything so instead of the usual theory, lets keep this blog going based upon an example.

## Superposition

Gates- Just like in boolean logic, gates take an input and then perform certain operations to it to eventually return the output. Similarly, the quantum gates are used to perform operations like superposition, entanglement on the qubits.

In the perspective of our example, it is like putting the coin through a magic machine that gives it some magical powers. to make it start spinning, now for an observer it is quite blurry to determine which side is heads or tails at a given time instant. This is referred as a superposition of states (in our case heads and tails), now extending the same concept to a personal computer that you are using to read this. A usual 8gb ram is ~ equivalent to 64 billion bits- where each bit stores the encoding data in 0's and 1's, now at best the current quantum computers have around 1000 qubits where each qubit can encode the data in a state of both 0's and 1's at a given time like the case of the spinning coin, now according to the common miconception that this makes it faster in performance in general- it doesn't, but yes in certain use cases this tremendously increases the computation power.

Mathematically, a single state is represented by the dirac notation of bra and ket.

```
∣0⟩=( 
1
0
​
 ),∣1⟩=( 
0
1
​
 )
```

A superposed state of both of them is written by:

```
∣ψ⟩=α∣0⟩+β∣1⟩
```

A physical representation to distinguish between the two of them can be through the bloch sphere diagram. The same state can also be expressed as:

```
∣ψ⟩=cos(θ/2)∣0⟩+e 
iϕ
 sin(θ/2)∣1⟩
```

Where theta is the angle from z axis and psi is from x axis in the x-y plane. To visualise a few states: |0> and |1> lie on the north and south pole of the sphere respectively.

The real power of computation with qubits comes when there are several of them- for example, imagine 2 qubits with their 4 basis states (00,01,10,11), 3 qubits with their eight states (000,001,010,100,011,110,101,111), n qubits with 2^n basis states.

*bit and qubit difference*

## Entanglement

Lets continue our example of the spinning coin but this time instead of a single coin we have two of them in the same motion, now after some time the coins will eventually come to rest and reveal the real values. In a normal scenario theres always a chance that both the coins will have the same observed value((HH or TT), but in our case if the coins are entangled with each other (connnected to each other by imagine a string) the outcome of one influences the other. Hence if coin A lands Heads then the coin B has to land Tails.

This helps in creating a superposition of multiple states - i.e, previously only the states 0 and 1 were superposed, now we can also superpose their basis states- 00 and 11 to get a measurement on the idea of both of them, in more complex and practical use cases this is what is done.

## Qiskit

Qiskit is a modern framework to implement the quantum computing principles in practice, developed by IBM in 2017.

Gates- Just like in boolean logic, gates take an input and then perform certain operations to it to eventually return the output. Similarly, the quantum gates are used to perform operations like superposition, entanglement on the qubits.

In the perspective of our example, it is like putting the coin through a magic machine that enriches it with some magical powers.

The starter code for all these test demos remains same as:

```python
!pip install qiskit
!pip install numpy
!pip install qiskit_aer

from qiskit import QuantumCircuit, transpile
from numpy import pi
from qiskit_aer import Aer
```

Few most commonly used gates are:

**1) H (Hadamard Gate)** - Used to superpose a given state into the superposed basis states.

```python
qc=QuantumCircuit(2,2) # 2 qubits and 2 classical bits to store their information
qc.h(0) #puts qubit 0 into superposition
qc.draw()
```

*superposed*

**2) X (Not Gate)**- Flips the given state to its conjugate (eg <0| to |1>).

```python
qc=QuantumCircuit(2,2)
qc.x(0) #flips it from 0 to 1
qc.draw()
```

*not*

**3) CX (Controlled Not Gate)**- Takes in 2 states as input, where one is the control gate, the other is the target. It maintains the state of the control qubit and flips the state of the target qubit.

```python
qc=QuantumCircuit(2,2)
qc.cx(1,0) # where 0 is the target bit and 1 is the control bit
qc.draw()
```

**4) RY (Rotational along Y axis gate)**- Rotates the angle of a qubit by an angle theta along the y axis.

**5) CRY(Controlled Rotation along Y axis gate)**- Applies the rotation along y axis on the target qubit only if the control qubit has state |1>.

```python
qc=QuantumCircuit(2,2)
qc.cx(pi/2,0,1 #(angle, control, target)
qc.draw()
```

*Blog image*

> **NOTE** - The Bell State is a special type that superposes and entangles a basis state at the same time, two most often used examples are:
> 
> ∣Ψ⁺⟩= ¹⁄√₂ (∣01⟩+∣10⟩)
> 
> ∣Ψ⁻⟩= ¹⁄√₂ (∣01⟩−∣10⟩)

Now lets summarize all these up together to get a basic circuit diagram.

```python
!pip install qiskit
!pip install numpy
!pip install qiskit_aer
from qiskit import QuantumCircuit, transpile
from numpy import pi
from qiskit_aer import Aer
qc=QuantumCircuit(2,2) #defining a quantum circuit
qc.h(0) #puts the qubit 0 into its superposed states
qc.cx(0,1) #entangles both qubits, with 0 as control and 1 as target
#^^^ initiates a bell state of psi+ ^^^
qc.ry(pi/4,0) #rotate qubit 0 by 45 degree
qc.cry(pi/2,1,0) #rotate qubit 0 by 90 degree only if qubit 1 in state 1
qc.draw()
```

The final circuit output is:

*output*

Now, after we put a coin in superposition/ entangle it's state, finally we have to stop it from spinning for determining what actually is the state. This is also called as measuring the circuit.

To get the measurements after collapsing a circuit, we either use the simulator or run it on an actual IBM computer. Practically it is very cost-effective and time-consuming to work on a real quantum computer, and almost every time people use simulators to test/run their projects. We will also use a qasm simulator for this project to simulate the measurements; there are other simulators also for different purposes. An example of simulator usage in our program above is:

```python
qc.measure([0, 1], [0, 1])  
simulator = Aer.get_backend('qasm_simulator')
final=transpile(qc, simulator)
job = simulator.run(final, shots=1024)
result = job.result()
counts = result.get_counts(qc)
print(counts)
```

Output: {'11': 71, '01': 77, '10': 404, '00': 472} -> represents the count of each basis state out of 1024, clearly showing how 00 comes in more times during measurement.

## References

All credits to the respective owners.

- [1) A gentle introduction to quantum computing](#)
- [2) Introduction to Qiskit](#)
- [3) Quantum Programming Language- interesting read](#)
- [4) Overview of Qubits](#)
- [5) Types of Quantum Computers](#)
