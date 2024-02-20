title = "Third Party Downtime"
timestamp = "2024-02-20"
draft = true
---
Every now and then I see a SaaS[^1] status page that explains that the current outage is caused by a third party of some sort. So far so good, thanks for letting me know. But what grinds my gears, and brings me to write this post, is the lack of responsibility usually taken for these outages.

Now in some cases there is legitimately nothing that can be done. When I was at CircleCI, it was no secret that the vast majority of build traffic came from GitHub. If parts of GitHub that are required for executing those builds went down, such as webhooks or certain APIs, there was not much we could do for those GitHub users other than wait for GitHub to restore service, and maybe recover some of the lost work.

But most cases are different. In the GitHub example the users chose to host their code on GitHub, not an unreasonable choice to be clear, and CircleCI wasn't the only service impacted when GitHub entered a degraded state. Importantly there was no way to make the feature of GitHub-triggered builds work if GitHub would not play along.

Also at CircleCI, we had physical Macs in a datacentre to run Mac builds.[^2] Sometimes this datacentre would experience disruptions, such as network problems. That was on us. We chose that specific datacentre. We chose[^3] to not have several datacentres with fail-over capabilities. The customer does not care where Mac builds come from, they just want them to work, and they pay us money for that. And when those disruptions became more frequent than we were comfortable with,[^4] we looked into actually doing something about them.

I led with this example because it makes the general point easier to digest. The same principle applies to AWS. If AWS goes down, and all your infrastructure is on there, you made that choice, probably for cost and complexity reasons. You could have also hosted on GCP. Supposedly some companies not yet owned by Microsoft also host on Azure.

But the solution is not to pick the single point of failure that is not down at the moment, the solution is redundancy, the ability to fail-over onto another system if the primary one fails. Of course there is cost involved with redundancy, but if you decide that that cost is not worth it, you do not get to throw your hands in the air when your single point of failure does eventually fail, as if there was nothing you could have done about this.[^5]

This is not news, it is literally [SLA Inversion](https://sookocheff.com/post/architecture/stability-antipatterns/#sla-inversion), every dependency is also a liability, because without any redundancy the system stops working as soon as any single dependency does. That is why any system in which reliability actually matters requires at least N simultaneous failures with N often ≥ 3, such as airplanes.[^6]


[^1]: Software as a Service

[^2]: This is because Apple are a bit special when it comes to licensing agreements and virtualisation. Essentially you can only run macOS VMs on macOS hosts, hardware and all. So you have a few hundred Mac Minis sitting in shelves somewhere.

[^3]: Presumably, I wasn't actually involved in any of this.

[^4]: You never buy 100% uptime, unless you're say NASA. Each nine added to uptime guarantees costs more than the last one, and you have to call it good enough at some point.

[^5]: In the German tech scene there is a meme, similar to the British [Computer says no](https://en.wikipedia.org/wiki/Computer_says_no), which loosely translates to "software problem, nothing we can do about it." It's frequently ridiculed because it exemplifies a behaviour of rejecting any responsibility as soon as technology is involved.

[^6]: Well, bit of a sore topic right now.
