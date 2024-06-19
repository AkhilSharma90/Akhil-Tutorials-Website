---
title: "Progress Tracker using React and Local Storage"
description: "How to install and use libraries"
icon: "code"
draft: false
---

## Overview
A Progress Tracker is a basic web application that lists tasks to be completed. Users can mark tasks as completed, and their progress is stored using the browser's local storage. The application includes a progress bar to display overall progress.

## Preview of Final Output
![Progress Tracker Preview](progress_tracker_preview.png)

## Technologies Used / Prerequisites
- **ReactJs**
- **Tailwind CSS**
- **JSX**
- **Functional Components in React**
- **React Hooks**
- **Local Storage**

## Approach
- **Containers**: Stateful React components (class-based).
- **Components**: Stateless React Components (function-based).
- We used **React Functional Components** and **React Hooks** (specifically `useState` and `useEffect`) to build the website.
- Components are written in **JSX** to render the UI and logic.
- All work is done using **React**, which operates on JavaScript.

## Project Structure
```
src/
|-- components/
|   |-- DSAList.js
|   |-- Section.js
|   `-- SubSection.js
|-- utils/
|   |-- calculateProgress.js
|   `-- dsaTrackerList.js
|-- App.js
|-- index.js
|-- index.html
```

## Steps to Create the Project

### Step 1: Set Up React Project
```sh
npx create-react-app progress-tracker
```

### Step 2: Navigate to the Project Folder
```sh
cd progress-tracker
```

### Step 3: Create Folder Structure
- Create a `components` folder for storing components.
- Create a `utils` folder for utility functions and initial state.

### Step 4: Add Tailwind CSS
Add the following script tag in the `index.html`:
```html
<script src="https://cdn.tailwindcss.com"></script>
```

## Code Implementation

### index.html
Automatically created file where we need to import the Tailwind CSS tag.

### index.js
Automatically created which React uses for final rendering.

### App.js
This file imports the `DSAList` component and exports it along with exporting the headings and name of the app.
```jsx
import DSAList from "./components/DSAList"; 

export default function App() { 
	return ( 
		<div className="flex flex-col justify-center items-center mt-4"> 
			<h1 className="text-emerald-500 font-bold text-3xl">GeeksforGeeks</h1> 
			<h3 className="bg-clip-text text-transparent bg-gradient-to-r from-purple-500 to-pink-500 font-bold text-xl mb-4">DSA Tracker</h3> 
			<DSAList /> 
		</div>
	); 
}
```

### DSAList.js
This file contains the overall logic of the website and all the required components.
```jsx
import { useState, useEffect } from "react"; 
import { findSectionProgress, findOverallProgress } from "../utils/calculateProgress"; 
import dsaTrackerList from "../utils/dsaTrackerList"; 
import Section from "./Section"; 

export default function DSAList() { 
	const [dsaList, setDsaList] = useState([]); 
	const [overallProgress, setOverallProgress] = useState(0); 

	useEffect(() => { 
		const localList = JSON.parse(localStorage.getItem("dsalist")) || []; 
		setDsaList(localList.length !== 0 ? localList : dsaTrackerList); 
	}, []); 

	useEffect(() => { 
		setOverallProgress(findOverallProgress(dsaList)); 
	}, [dsaList]); 

	const updateList = (index, indexOfSub) => { 
		const newDSAList = [...dsaList]; 
		newDSAList[index].subsections[indexOfSub].completed = !newDSAList[index].subsections[indexOfSub].completed; 
		newDSAList[index].progress = findSectionProgress(newDSAList[index].subsections); 
		setDsaList(newDSAList); 
		localStorage.setItem("dsalist", JSON.stringify(newDSAList)); 
	}; 

	return ( 
		<div className="flex flex-col gap-10 w-[60%] mb-40 relative"> 
			{overallProgress === 100 && <h1 className="text-center text-4xl text-emerald-500">Successfully Completed! Hurray.</h1>} 
			<p>Progress: {overallProgress}%</p> 
			<div className={`-mt-5 rounded sticky top-0 bg-gradient-to-r from-purple-500 to-pink-500 transition-all h-2 w-[${overallProgress}%]`}></div> 
			{dsaList.map((section, index) => ( 
				<Section index={index} updateList={updateList} key={index} section={section} /> 
			))} 
		</div> 
	); 
}
```

### Section.js
This component defines the UI and logic of individual sections.
```jsx
import { useState } from "react"; 
import SubSection from "./SubSection"; 

export default function Section({ section, index, updateList }) { 
	const [open, setOpen] = useState(false); 
	return ( 
		<div className="bg-gray-200 px-10 py-6 w-full text-slate-800 rounded shadow-lg transition hover:shadow-2xl hover:-translate-y-2 hover:scale-[101%]"> 
			<div className="flex w-full justify-between items-center cursor-pointer"> 
				<h3 className="font-bold text-xl flex-1" onClick={() => setOpen((prev) => !prev)}>{section.title}</h3> 
				<div className="flex gap-4 items-center"> 
					<p className="font-bold text-slate-800">{section.progress}%</p> 
					<button onClick={() => setOpen((prev) => !prev)} className="bg-gray-800 text-white px-5 py-3 rounded hover:bg-gray-600">{open ? "Close" : "Open"}</button> 
				</div> 
			</div> 

			{open && ( 
				<div className="flex flex-col w-full my-10 gap-4"> 
					{section.subsections.map((sub, i) => ( 
						<SubSection key={i} index={i} sectionIndex={index} updateList={updateList} subtitle={sub.subtitle} completed={sub.completed} /> 
					))} 
				</div> 
			)} 
		</div> 
	); 
}
```

### SubSection.js
This component defines the UI and logic of individual sub-sections.
```jsx
export default function SubSection({ subtitle, completed, index, sectionIndex, updateList }) { 
	return ( 
		<div className="flex w-full justify-between items-center"> 
			<h4 className="font-bold text-lg"><span className="inline-block mr-4">{index}.</span> {subtitle}</h4> 
			<input 
				onChange={() => { updateList(sectionIndex, index); }} 
				checked={completed || false} 
				type="checkbox" 
				className="border rounded w-4 h-4 accent-emerald-500" 
			/> 
		</div> 
	); 
}
```

### dsaTrackerList.js
This file stores the initial state of the progress object.
```js
const dsaTrackerList = [ 
	{ 
		title: "Arrays", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Introduction to Arrays", completed: false }, 
			{ subtitle: "Array Operations", completed: false }, 
			{ subtitle: "Common Array Problems", completed: false }, 
		], 
	}, 
	{ 
		title: "Linked Lists", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Singly Linked Lists", completed: false }, 
			{ subtitle: "Doubly Linked Lists", completed: false }, 
			{ subtitle: "Linked List Operations", completed: false }, 
		], 
	}, 
	{ 
		title: "Stacks", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Introduction to Stacks", completed: false }, 
			{ subtitle: "Stack Operations", completed: false }, 
			{ subtitle: "Applications of Stacks", completed: false }, 
		], 
	}, 
	{ 
		title: "Queues", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Introduction to Queues", completed: false }, 
			{ subtitle: "Queue Operations", completed: false }, 
			{ subtitle: "Applications of Queues", completed: false }, 
		], 
	}, 
	{ 
		title: "Trees", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Binary Trees", completed: false }, 
			{ subtitle: "Binary Search Trees", completed: false }, 
			{ subtitle: "Tree Traversal", completed: false }, 
			{ subtitle: "Balanced Trees", completed: false }, 
		], 
	}, 
	{ 
		title: "

Sorting Algorithms", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Bubble Sort", completed: false }, 
			{ subtitle: "Insertion Sort", completed: false }, 
			{ subtitle: "Merge Sort", completed: false }, 
			{ subtitle: "Quick Sort", completed: false }, 
		], 
	}, 
	{ 
		title: "Graphs", 
		progress: 0, 
		subsections: [ 
			{ subtitle: "Graph Representation", completed: false }, 
			{ subtitle: "Graph Traversal", completed: false }, 
			{ subtitle: "Shortest Path Algorithms", completed: false }, 
			{ subtitle: "Minimum Spanning Tree", completed: false }, 
		], 
	}, 
]; 

export default dsaTrackerList;
```

### calculateProgress.js
This file is used to calculate progress made by the user in each topic along with the overall progress.
```js
export function findSectionProgress(subsections) { 
	let completed = 0; 
	for (let i = 0; i < subsections.length; i++) { 
		if (subsections[i].completed) completed++; 
	} 
	return Math.round((completed / subsections.length) * 100); 
} 

export function findOverallProgress(dsalist) { 
	let totalProgress = 0; 
	for (let i = 0; i < dsalist.length; i++) { 
		totalProgress += dsalist[i].progress; 
	} 
	return Math.round((totalProgress / (dsalist.length * 100)) * 100); 
}
```

## Conclusion
This guide provides a step-by-step approach to building a progress tracker using React and local storage. By following the steps and utilizing the provided code snippets, you can create a functional and visually appealing progress tracker for any task or project.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
