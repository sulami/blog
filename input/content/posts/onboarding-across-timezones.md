title = "Onboarding Across Timezones"
timestamp = "2021-05-06"
tags = ["staff"]
---
Even in a fully distributed organisation, teams are often clustered in timezones to facilitate collaboration. Another model is the deliberate spreading of teams to enable *[Follow the Sun](https://en.wikipedia.org/wiki/Follow-the-sun)* workflows, which can also improve pager rotations.

In a distributed team, onboarding new team members can be especially difficult. New onboardees lack the context and institutional knowledge required to work effectively in an independent manner. Here are some strategies which I have found ease this process, specifically in the field of software engineering.[^1]


# Onboarding Buddies

It is standard practice to designate an onboarding buddy for a new onboardee, a person in the same team who can take care of them during the first few weeks, answer questions, and pair a lot. It is not always possible to have this onboarding buddy in the same timezone, and in these cases it is a good idea to have an existing employee in the same timezone as a designated point of contact, regardless of team affiliations.


# Making the Most of the Overlap

When onboarding someone in different timezones, everything has to be planned around the overlap in office hours. Start by clearly identifying this overlap. It is most important for the onboardee to be able to onboard and work effectively outside of this overlap.

To optimise the value gotten out of the overlap, it is best spent with synchronous conversations. Other tasks such as code reviews or planning work should be moved outside the overlap unless there is value in doing it synchronously, for example by sharing additional context.

If the overlap is very small, short screencasts can be medium-cost, high-bandwidth method to transfer context asynchronously.[^2] These can be recorded and watched outside the overlap, but convey much of the same context as pairing could, albeit without the ability to actually interject questions.


# Enabling Self-Directed Learning

To enable the onboardee to learn on the job, there needs to be a pool of work items for them to pick up tasks from. These items need be narrowly scoped and include much more detail and context than they ordinarily would, to reduce the risk of the onboardee getting stuck.

Tasks should be sized "just right," that is not larger than two or three days, but also not shorter than a single day. Very small tasks encourage the onboardee to start working on several tasks in parallel while waiting for code reviews, increasing cognitive load.

A library of self-directed training material is a must-have for every organisation,[^3] distributed or not, but can also serve as a fallback if the onboardee gets stuck on their current task and has to wait for the next overlap.


# Just Because You Can Does Not Mean You Should

While asking colleagues for ideas & feedback, a sentiment I have heard more than once was "if you can, avoid scattering your team." Just because you can spread a team literally around the globe does not mean you should do this. At my current day job we have employees around the globe, but generally try to keep teams within two timezone regions, for example within the North American east coast and Europe, or the west coast and APAC, so that we have an overlap of at least two to three hours.


[^1]: Though many of the concepts should translate reasonably well to other fields.

[^2]: I'm normally not a big fan of video as a medium because it is inherently difficult to index/search and impossible to edit to keep up to date with future changes. As such, I still wouldn't recommend storing these in an archive, but treating them as information snapshots in time, just like the synchronous conversation they're meant to replace.

[^3]: I can't stress this point enough. When I joined CircleCI onboarding was done almost exclusively through onboarding buddies' hand-holding and bite-sized tasks. For obvious reasons this doesn't scale very well, and introduces additional strain on the existing team every time a new person joins. We've since introduced such a library which has reduced onboarding overhead significantly.
