# Experiments and Causality: W241 Final Project

# Gender and Racial Bias (U.S. Coast Guards)

## Introduction

The US Coast Guard has experienced an organizational reckoning over the last ten years. Spurred in part by a parallel societal reckoning and several high profile and embarrassing incidents at the US Coast Guard Academy (USCGA), a four year college and primary Coast Guard officer accession source, the US Coast Guard (CG) has confronted the reality that as a service, it’s simply not diverse, nor inclusive enough. Several high profile studies recommended strategies to cultivate a more inclusive workforce, which the CG implemented or is in the process of implementing. But have those strategies impacted the mindsets of the rank and file Officer Corps? This experimental study aims to answer the following question: Do gender and race impact Coast Guard officers’ perception of other officers?  

In order to stage and iterate our experiment, we first conducted the experiment using [Berkeley's XLab](https://xlab.berkeley.edu/) resources. After summarizing our analysis and coming up with an end to end report for the pilot, the group aims to re-conduct the analysis against the U.S. Coast Guard participants.

## Team

1. Hanyu Dai
2. Brendan Mattina
3. Abhi Sharma

Advisor: Douglas Alex Hughes (Professor, U.C. Berkeley)
 
## High Level Experiment Design

The experiment uses a "Differences in Differences" (DiD) design to answer the causal question of gender/racial bias. There are 2 genders (male and female) and 2 races (white and black) that are chosen as "levels" for this experiment. These levels are interacted with each other to generate a multi factor designed experiment that uses 4 subjects as treatment conditions. For example, interacting gender and race gives us: 

1. White Male
2. Black Male
3. White Female
4. Black Female 

To scope the complexity of the design and to keep the survey short, participants are shown 6 sets of resumes for comparison. Each set contains 2 resumes. Within each set shown to a participant, they are required to compare resume summaries of 2 people.

For the control group, the 2 resumes "in the set" are shown without identifying information such as name, gender and race. The control group aims to establish the difference in the perceived quality of the resume summaries shown to the respondent.

![image](https://user-images.githubusercontent.com/10823325/114312083-3935ae00-9aa6-11eb-9f28-69dddb5c3576.png)

For the treatment group, the 2 resumes "in the set" are shown with identifying information (gender and race). This is captured by administering a treatment - such as the person's name - in the heading of the resume summary. For example, the treatment group receives a set of resume summaries - one for Bradley Meyer, another for Reginald Washington.

![image](https://user-images.githubusercontent.com/10823325/114311896-7b122480-9aa5-11eb-9905-9a411167ccb5.png)

Ultimately, the average treatment effect is captured when we take the difference between the control and treatment group (for each resume set):
![image](https://user-images.githubusercontent.com/10823325/114312022-0c819680-9aa6-11eb-80cf-2e356174e36c.png)

## Pilot Data

In order to run the experiment at a pilot stage before we officially run it with the Coast Guard, we used [Berkeley's XLab](https://xlab.berkeley.edu/) resources to help us with survey design and initial analysis. This repository captures data and analysis from the xLab survey. 

The pilot data captures responses from an audience that is not expected to have much covariate similarity (both measured and omitted covariates) compared to the U.S. Coast Guards. It is entirely possible that we find similar, opposite or insignificant results when the survey is redone with the Coast Guards. However, we hope to maintain a common theme of analysis that can be applied to both groups.

## Links and Resources

Team Google Drive: [link](https://drive.google.com/drive/folders/18yaVOAfwbdqiGPOwfXl0aif_kf5AELBB?usp=sharing)

Here is the official survey that was sent to the xLab participants: [link](https://berkeley.qualtrics.com/jfe/form/SV_eJ75fXEEK5gMHZQ)
