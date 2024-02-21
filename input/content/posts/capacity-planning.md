title = "Capacity Planning Handbook"
slug = "capacity-planning"
timestamp = "2022-05-11"
tags = ["staff", "best-of"]
---
*I wrote this at work, and am now sharing it more widely so that others might benefit from it.*

---

Assuming we have a service we want to perform capacity planning for, we answer these questions in order:

1.  What are the performance indicators of the service?
2.  What are the components influencing service capacity?
3.  What are the core drivers of service load and their current value?
4.  What is the primary resource, and how does it scale with the core drivers?
5.  How much is usage going to grow organically?
6.  How much additional growth is planned?
7.  How much headroom is required?
8.  How much capacity is required?

It is important to reassess the answers regularly. As we change our system, its load and performance characteristics change, and thus the answers to many of those questions and all that follow will change as well.


# What are the performance indicators of the service?

The first step is to find out how to measure **quality of service**. This is likely expressed in latencies and error rates, and can often be taken directly from SLIs/SLOs. At this stage we are not trying to define system load yet, but just how well the system is performing as an opaque box given a specific load and **capacity**, neither of which we measure at this point. A second question to answer here is “when is the service working as intended?” For each indicator we need a desired value (range) in which the service is considered operational within spec. For a latency or error rate this would typically be a maximum value.


# What are the components influencing service capacity?

A service often consists of several components, such as the primary deployment(s), underlying data stores, queues, caches, etc. List out all of the components that can be scaled horizontally and/or vertically independently of the other components.

**Going forward answer all questions for each component individually.**


# What are the core drivers of service load and their current value?

We then find out what the underlying **core drivers** for system load are, which is how we measure **usage** of the service. This might be the number of pipelines or jobs started, the number of API requests, etc. Depending on our system the drivers might be closer to the end-user or closer to the system (e.g. public API requests vs. inter-service RPC).

Make sure that our **core drivers** actually encompass most if not all the load on the service. Many services receive load from different interactions, in our case often work throughput and API request volume, which happen to mostly overlap, but are independent drivers.


# What is the primary resource, and how does it scale with the core drivers?

The **primary resource** is the one resource that limits a component’s further scaling. To find the primary resource, compare the history of resource metrics with the one of the core drivers to find correlations. If we want to be fancy we can also use linear regression. Once we have the resources that are being consumed by an increase of the core drivers, determine which one would hit its limit first as load increases. That is our **primary resource**.

Depending on the data available we might need to perform manual load tests with a representative load profile to determine where we actually hit the limits. Some resources are very clearly limited, e.g. provisioned IOPS on a database, whereas others have a performance cliff at an unknown point, e.g. we can rarely get to 100% CPU usage on an application, performance will often drop dramatically at some point before that.

The goal here is to establish a relationship between the core drivers and the primary resource, so that we can find our **capacity limit**, the point where increasing a core driver will cause a degradation of service quality beyond what is acceptable. Secondary resources can also be worth tracking, as they can become primary resources as load and performance characteristics change.

Some inspiration for resources:

-   CPU time
-   Memory
-   Disk space
-   Disk throughput
-   Network throughput
-   Thread pools (though usually limited by either CPU or memory)


# How much is usage going to grow organically?

Based on metrics from the past we can usually establish an **organic growth rate**, which might be a linear function, or perhaps an exponential rate function. Depending on the time frame we looking at we will want to estimate growth for the next 3-24 months.


# How much additional growth is planned?

Sometimes we can anticipate additional growth because we are planning a migration or releasing a much requested feature. In this case we should factor in how much growth we expect from those events.


# How much headroom is required?

If there are known spikes in usage, we need to account for them. Are the spikes fixed numbers or a percentage of the regular usage?

Beyond expected spikes in usage, we will also want some headroom to deal with unexpected spikes, e.g. incidents tend to result in a backlog of work that is worked through at a higher rate than usual.


# How much capacity is required?

This is the point where we bring everything together. We know how much load on the component is created by a certain usage in terms of **primary resource usage** per **core driver** value. We know how much of the resource(s) we have available, and thus how much load the component can take based on the **capacity limits**. We have an estimate of how we expect the **core driver** and thus the **resource usage** to develop in the future.

The formula for each component’s **resource** is:

```python
required capacity = current usage * (organic growth + planned growth + headroom) * resource required per usage
```

For example we might find out that each 100 pipelines/minute require 250 IOPS on our database, and that is the only resource we are anywhere near using up as we grow. If we are currently running 800 pipelines/minute and expect that to grow by 25% of the next 6 months, and would like to have 15% headroom, we get:

```python
800 pipelines * 1.25 growth * 1.15 headroom * 250 IOPS/100 pipelines = 2875 IOPS required
```

We can compare that to the current IOPS we have provisioned and adjust as necessary.

We then repeat this for all components, and potentially for any secondary resources that we might also exhaust. We then document those numbers, so that we can repeat the calculation in the future.


# Further reading

-   [The Practice of Cloud System Administration](https://the-cloud-book.com/) has been the source of a lot of the above, and is generally a pretty good and comprehensive read.
-   [The SRE workbook](https://sre.google/workbook/table-of-contents/) covers a lot of adjacent topics.
