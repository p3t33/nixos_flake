''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

  const steps = [
    {
      name: "Evaluation",
      prompt: `You are building a learning plan. This is Step 1: Evaluation.

Elicit everything needed through structured questioning. Cover these 7 themes in order, one question at a time:

1. Domain — what are they learning
2. Horizon — bounded (test prep, certification, specific project) or unbounded (career skill, ongoing craft). Explain the distinction with examples and get a clear answer.
3. Success criteria — what does done look like
4. Baseline — what do they already know. Probe for specifics.
5. Assets — tools, hardware, resources already acquired
6. Constraints — time, access, deadlines, scope boundaries
7. Concerns — bottlenecks, worries, known risks

Rules:
- One question at a time. Wait for the answer.
- Every answer must include concrete specifics. Follow up if vague.
- For each question, provide your recommended answer so the user has something to react to.
- When all 7 themes are resolved, summarize all 7 with their answers and ask the user to approve before finishing.`,
    },
    {
      name: "Blind Spots",
      prompt: `This is Step 2: Blind Spots.

Review the full evaluation from Step 1 as a single body of information. Surface critical gaps the user didn't think of — things they couldn't self-assess because they don't know the domain well enough yet.

- Identify blind spots that would block or seriously hinder progress
- Present findings ranked by criticality with a clear recommendation for each
- Show reasoning for every finding — or for finding nothing
- Ask a focused question or two if needed to resolve ambiguity
- Do not loop back into evaluation. Focus on what's missing, not what's wrong.`,
    },
    {
      name: "Curation",
      prompt: `This is Step 3: Curation. Build the complete learning plan.

Operating principles:
1. Hands-on over passive study — always. Anchor every phase on building, practicing, or producing something real.
2. Maximize return on time invested. Match medium to concept. Ruthlessly cut anything that doesn't deliver outsized value.
3. One thing drives, everything else is pulled. Each phase has exactly one driver resource.

Structural rules:
- Phases revisit and deepen core concepts through new material at increasing depth.
- Guided first phase, then unguided with progressively less scaffolding.
- One driver per phase. Other resources are pull-based.
- Start flat, branch into subtracks only when it materially improves sequencing.

Plan sections:
1. Purpose & Goals
2. Current Baseline (provisional for unbounded, with reassessment triggers)
3. Scope — in/out
4. Blind spots carried from Step 2
5. Tracks and phases

Each resource must include: Name, Medium, Why it fits, Where in sequence, Role (driver or pull-based).

Generate the plan now. Do not ask for confirmation before writing.`,
    },
    {
      name: "Review",
      prompt: `This is Step 4: Review. Audit the plan from Step 3.

Run these checks:
1. Alignment — does the plan address goals, baseline, and constraints from evaluation?
2. Gaps — fundamental resources conspicuously absent?
3. Medium — would a different medium teach any concept faster?
4. Repetition — do phases revisit core concepts at increasing depth? Fix isolated silos.
5. Resource verification — for every resource, verify it exists using fetch_url. If unverifiable, find a replacement or flag the gap.
6. Operating principles — every phase anchored on hands-on work? Every resource maximizes time ROI?

Fix any issues found. Present the updated plan if changes were made.`,
    },
    {
      name: "Output",
      prompt: `This is Step 5: Output.

Write the final learning plan to a markdown file in the current working directory.
File name format: {domain}-learning-plan.md

Confirm the file path when done.`,
    },
  ];

  export default function (pi: ExtensionAPI) {
    pi.registerCommand("learning-planner", {
      description: "Build a structured learning plan through guided steps",
      handler: async (args, ctx) => {
        if (!args) {
          ctx.ui.notify("Usage: /learning-planner <what you want to learn>", "warning");
          return;
        }

        ctx.ui.notify("Starting learning planner", "info");

        // Step 1: Evaluation — interactive, model drives the Q&A
        await ctx.sendUserMessage(
          steps[0].prompt + "\n\nThe user wants to learn: " + args
        );
        await ctx.waitForIdle();

        // Gate between each subsequent step
        for (let i = 1; i < steps.length; i++) {
          const step = steps[i];
          const proceed = await ctx.ui.confirm(
            "Step " + (i + 1) + ": " + step.name,
            "Proceed to " + step.name + "?"
          );
          if (!proceed) {
            ctx.ui.notify("Stopped after Step " + i + ": " + steps[i - 1].name, "info");
            return;
          }

          await ctx.sendUserMessage(step.prompt);
          await ctx.waitForIdle();
        }

        ctx.ui.notify("Learning plan complete", "success");
      },
    });
  }
''
