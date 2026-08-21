---
title: "Syllabus"
layout: page
permalink: /syllabus/
format:
  pdf:
    pdf-engine: xelatex
---

## Course Information

- **Location:** Eddy 106  
- **Time:** Tuesday and Thursday, 9:30–10:45 AM  
- **Term:** Fall 2026 (August 25–November 19)
- **Final Exam:** Thursday, November 19, during the regularly scheduled class period
- **Instructor:** Jude Bayham  
  - Office: Nutrien 203  
  - Email: [jbayham@colostate.edu](mailto:jbayham@colostate.edu)  
  - Office Hours: Monday and Wednesday 1–2, or by appointment ([book here](https://outlook.office.com/bookwithme/user/8f601447c7934b3fb7dc895f4140ffe4@colostate.edu?anonymous&ismsaljsauthenabled&ep=plink))  


## Course Description

This course provides graduate students in agricultural, environmental, and resource economics with an introduction to optimization methods used in economic modeling and policy analysis. Emphasizing both theory and computation, the course covers linear and nonlinear programming, dynamic optimization, and numerical methods essential for solving complex economic problems. Students will learn to formulate, solve, and interpret optimization models using R, with applications drawn from land use planning, natural resource management, agricultural production, and environmental policy design. The course equips students with practical modeling skills and analytical tools that support evidence-based research and decision-making in applied economics.


## Learning Objectives

By the end of this course, students should be able to:

1. Formulate and solve numerical optimization models in economics.
2. Use optimization software to solve economic optimization problems (with a focus on R).
3. Interpret results from mathematical programming models in applied settings.
4. Use optimization models to conduct independent research.

## Prerequisites

AREC 506 or equivalent. I assume students are familiar with basic microeconomic theory (e.g., producer, consumer, general equilibrium) and constrained and unconstrained optimization.

## Textbooks and Readings

I will post several texbooks on Canvas. These are references for the course content.

- Hazell, Peter B. R., and Roger D. Norton. *Mathematical Programming for Economic Analysis in Agriculture*.
- McCarl, Bruce A., and Thomas A. Spreen. *Applied Mathematical Programming Using Algebraic Systems*. [Available online](https://agecoresearch.tamu.edu/mccarl/regbook/)
- Miranda, Mario J., and Paul L. Fackler. *Applied Computational Economics and Finance*.


Supplemental readings and journal articles will be assigned.

## Assignments and Grading

| Component                | Percentage |
|--------------------------|------------:|
| Midterm Exam 1          | 20%        |
| Midterm Exam 2          | 20%        |
| Final Exam              | 20%        |
| Model Labs               | 40%        |

I will use the standard CSU grading scale including + and - where the thresholds are defined at 2 and 8 of each 10-point range.

### Exams

There will be two midterm exams and a final exam. The exams will be written, in class, and will not require students to run code. They will assess formulation, interpretation, numerical judgment, and small transparent calculations. The final will be cumulative, with emphasis on applying and interpreting the course's linear, nonlinear, and discrete-time dynamic models.

### Model Labs

Model Labs replace a traditional semester project with a series of short-turnaround, AI-assisted applied optimization exercises. The purpose is to investigate economic models more deeply than a conventional short assignment permits while remaining responsible for understanding, validating, and explaining the work. Evaluation will emphasize understanding of the optimization model; correctness of the implementation and reasoning; economic and mathematical interpretation; the ability to explain and defend the work.

On selected Thursdays, we will examine an applied economics model or paper. Whether a model or paper, we will develop or identify the economic decision, decision variables, objective function, constraints, parameters, data, and endogenous outcomes. We will also consider the mathematical properties and solution concepts that matter for the model—including, as appropriate, first-order conditions, shadow prices, complementary slackness, and uniqueness—and the assumptions that make the model useful or restrictive. 

Between Thursday and Tuesday, each student will use AI and other computational tools to conduct a focused computational or conceptual experiment that produces one substantive insight. A lab may reproduce a simplified model or numerical result; perturb a parameter, constraint, or objective coefficient; identify a problematic parameterization or assumption; extend the model with an economically meaningful feature; reformulate the economic problem; or explain an important feature through a derivation, visualization, or numerical experiment. Labs will become progressively less structured: early exercises may use a supplied working model, while later exercises may provide a partial implementation, data, or the original paper and ask students to formulate and investigate a new question.

AI tools may be used extensively for coding, derivations, debugging, interpretation, and brainstorming. Students remain responsible for their assumptions, formulation, code, results, and claims. 

On designated Tuesdays, every student must arrive prepared to explain the investigation. At the beginning of class, approximately two or three students will be selected at random to give concise presentations organized around three questions: **What did you want to understand? What did you change, build, solve, or test? What did you learn, and why did the result occur?** Presenters should briefly summarize the key change and teach the class how it affects the solution. The instructor will ask follow-up questions that require reasoning directly from the model, such as why a constraint binds, what a parameter change would imply, the economic meaning of a multiplier, whether a solution is unique, or which assumption drives a result.

Students who are not selected to present are still expected to participate. The instructor may select respondents to predict another comparative static, explain an implication, suggest a useful modification, identify a driving assumption, connect the result to an optimization concept, or propose a next experiment. These discussions are intended to make Model Labs a class-wide model investigation rather than a sequence of passive presentations.

You will submit your slides as the deliverable.






## Tentative Schedule of Topics

1. **Modeling Basics**
   - Credible economic models, R, and AI-assisted model development
   - Objectives, decision variables, constraints, units, and solution audits

2. **Linear and Mixed-Integer Programming**
   - Formulation, graphical intuition, and matrix representations
   - Solving and interpreting applied linear programs
   - Sensitivity analysis, shadow values, and duality
   - Binary choices, fixed costs, and mixed-integer applications

3. **Static Nonlinear Optimization**
   - Numerical foundations: roots, diagnostics, and convergence
   - Unconstrained optimization, derivatives, geometry, and local/global optima
   - Constrained optimization, feasible sets, bounds, corners, and KKT conditions
   - Solver initialization, scaling, feasibility, and interpretation of multipliers
   - Risk, uncertainty, and scenario analysis

4. **Dynamic Optimization**
   - States, controls, transitions, payoffs, and discounting
   - Finite-horizon dynamic programming and backward induction in R
   - Infinite-horizon Bellman equations and value function iteration
   - Value and policy functions, diagnostics, and parameter sensitivity


## Principles of Community

The Principles of Community support the Colorado State University mission and vision of access, research, teaching, service and engagement. A collaborative, and vibrant community is a foundation for learning, critical inquiry, and discovery. Therefore, each member of the CSU community has a responsibility to uphold these principles when engaging with one another and acting on behalf of the University

*Inclusion*: We create and nurture inclusive environments and welcome, value and affirm all members of our community, including their various identities, skills, ideas, talents, and contributions.

*Integrity*: We are accountable for our actions and will act ethically and honestly in all our interactions.
Respect: We honor the inherent dignity of all people within an environment where we are committed to freedom of expression, critical discourse, and the advancement of knowledge.

*Service*: We are responsible, individually and collectively, to give of our time, talents, and resources to promote the well-being of each other and the development of our local, regional, and global communities.

*Social Justice*: We have the right to be treated and the responsibility to treat others with fairness and equity, the duty to challenge prejudice, and to uphold the laws, policies and procedures that promote justice in all respects.

## Academic Integrity

Academic misconduct (see examples below) undermines the educational experience at Colorado State University, lowers morale by engendering a skeptical attitude about the quality of education, and negatively affects the relationship between students and faculty/instructors.

Faculty/Instructors are expected to use reasonably practical means of preventing and detecting academic misconduct. Any student found responsible for having engaged in academic misconduct will be subject to academic penalty and/or University disciplinary action.

Students are encouraged to positively impact the academic integrity culture of CSU by reporting incidents of academic misconduct.

Examples of academic misconduct include (but are not limited to):

1.	Cheating – Cheating includes using unauthorized sources of information and providing or receiving unauthorized assistance on any form of academic work or engaging in any behavior specifically prohibited by the instructor in the course syllabus or class presentation.

2.	Plagiarism – Plagiarism includes the copying of language, structure, images, ideas, or thoughts of another, and representing them as one’s own without proper acknowledgment, and is related only to work submitted for credit. Also included is the failure to cite sources properly; sources must always be appropriately referenced, whether the source is printed, electronic or spoken.

3.	Unauthorized Possession or Disposition of Academic Materials – Unauthorized possession or disposition of academic materials includes the unauthorized selling or purchasing of examinations, term papers, or other academic work; stealing another student’s work; and using information from or possessing exams that an instructor did not authorize for release to students.

4.	Falsification – Falsification encompasses any untruth, either verbal or written, in one’s academic work.

5.	Facilitation of any act of Academic Misconduct – Facilitation of any act of academic misconduct includes knowingly assisting another to commit an act of misconduct.

## AI Statement

This course treats generative AI as a working tool, and your ability to use it well is part of what is being assessed. 

You are expected to use AI tools on the model labs. You will be evaluated not on whether you used them, but on how well you directed them, how accurately you judged their output, and how effectively you corrected what they got wrong. I won't ask you for your prompts, but I may ask you about your reasoning, your assumptions, and your interpretation of the results.

During class, I expect that you don't use computers unless you are presenting. I urge you to take hand written notes whether on paper or a tablet.

You may also use AI tools to help you prepare for exams, but you will not be allowed to use them during the exams themselves.

You remain responsible for the accuracy of everything you submit or present. "The model produced it" is not a defense here any more than it is anywhere else. 

AI-enabled assistive technology authorized by a student's accommodation letter is permitted in this course. 


## Accommodations

If you require accommodations, contact the Student Disability Center:  
[https://disabilitycenter.colostate.edu](https://disabilitycenter.colostate.edu), TILT Room 121, (970) 491-6385.
