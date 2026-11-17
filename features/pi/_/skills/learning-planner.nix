''
  ---
  name: learning-planner
  description: "Builds a complete learning plan with curated resources for any domain. Elicits domain-specific details through structured questioning, applies fixed learning principles, and produces a persistent plan artifact. Use via /skill:learning-planner when you want a full learning plan."
  ---

  # Learning Planner

  Build a complete, actionable learning plan with curated resources for any domain — from grilling meat to embedded development to differential math.

  ## Learning Model

  Six principles that govern every decision in this skill. Every structural choice, resource selection, and review check must be justified against these principles.

  1. **Transfer-appropriate practice** — practice mirrors the goal. The learner does the actual activity, not reads about it.
  2. **Pull-based supporting knowledge** — no prerequisite stages. Pull knowledge at point of need when practice surfaces a gap.
  3. **Deliberate effort builds patterns** — learning is building a pattern library through System 2 engagement. Mastery is when patterns become automatic (System 1). Practice must keep the learner in System 2 — if it's automatic, it's not learning.
  4. **Interleaved practice** — vary across topics to strengthen pattern discrimination.
  5. **Spaced repetition** — space practice to consolidate into long-term memory.
  6. **Timely feedback in a high-validity environment** — clear feedback, close to the action, in an environment where real patterns exist.

  ## Step 1: Evaluation

  **Purpose:** Elicit everything needed to build the plan through structured questioning.

  **Guiding principles:**

  - One question at a time. Wait for the answer before proceeding.
  - Every answer must include concrete specifics — what they've done, built, read, or used. If it doesn't, follow up until it does.
  - Confirm each theme is resolved with a specific answer before moving to the next.
  - For each question, provide your recommended answer so the user has something to react to.
  - Never convert a knowledge gap into a prerequisite goal. Gaps are addressed through practice of the primary goal — pull knowledge at point of need, not before it.
  - Extract only. Save all recommendations for Step 4.
  - Thoroughness over speed. A bad plan costs more time than thorough questions.
  - Always ask explicitly. Every theme is resolved by the user's words, not the agent's inference.

  **Process:**

  Cover these themes in order:

  1. **Goal** — a concrete activity the user wants to be able to perform. Cook a steak, not "become a chef." Board bringup from power-on to booting Linux, not "learn embedded." Pass a differential equations test, not "learn differential equations." If the user's goal is broad or identity-based, refine it to the single most impactful activity given their current baseline. Other activities become future tracks, not prerequisites.
  2. **Horizon** — bounded or unbounded. Bounded: test prep, certification, specific project with a finish line — tighter plans, fewer passes, retention targeted to a horizon. Unbounded: career skill, ongoing craft, long-term capability — review checkpoints, reassess-baseline triggers, built for long-term retention. Explain the distinction with examples and get a clear answer — this is a structural decision that shapes the entire plan.
  3. **Success criteria** — what does done look like, minimum bar.
  4. **Baseline** — what do you already know. Probe for specifics: what have you done, built, read, used. Knowledge gaps discovered here are context for resource selection — they are never converted into prerequisite goals or separate stages. Gaps are addressed through practice — pull knowledge at point of need when practice surfaces a gap. If the gap between baseline and goal is so large that the learner cannot meaningfully attempt even a guided practice unit — they lack the minimum vocabulary to engage with the activity — the goal is rescoped closer to the baseline. This is not a prerequisite stage; it is a different, appropriately-scoped goal.
  5. **Assets** — tools, hardware, resources already acquired.
  6. **Constraints** — time, access, deadlines, scope boundaries.
  7. **Concerns** — bottlenecks, worries, known risks.

  **Exit criteria:** All 7 themes have concrete, specific answers. No theme was skimmed or left vague. The user and skill have explicit agreement on a single concrete goal stated as an activity that can be practiced directly. Summarize all 7 themes and their resolved answers back to the user before proceeding. The user explicitly approves or iterates.

  ## Step 2: Blind Spots

  **Purpose:** Take resolved themes from Step 1, zoom out, and surface critical gaps the user didn't think of.

  **Guiding principles:**

  - Focus on what's missing, not what's wrong. The evaluation answers are taken as given.
  - Surface all blind spots found. The user opts out, not in.
  - Show reasoning for every finding — or for finding nothing. Every conclusion requires evidence.

  **Process:**

  - Review the full evaluation as a single body of information.
  - Identify blind spots that would block or seriously hinder progress — things the user couldn't self-assess because they don't know the domain well enough yet.
  - Present findings ranked by criticality with a clear recommendation for each.
  - Ask a focused question or two if needed to resolve ambiguity.
  - Note findings and carry forward. Do not loop back into Step 1.

  **Exit criteria:** Present refined themes — Step 1 output with blind spot findings integrated, gaps filled, risks surfaced with recommendations. This replaces Step 1 output as the single source of truth for all downstream steps. The user explicitly approves or iterates.

  ## Step 3: Structure

  **Purpose:** Propose the track/phase skeleton based on refined themes from Step 2. No resource selection — only the shape of the learning journey.

  **Process:**

  Hierarchy:

  - **Track** — a self-contained learning journey toward the concrete activity goal established in evaluation and refined by blind spots.
  - **Phase** — the atomic practice unit. One complete iteration of doing the thing the learner is trying to learn. A phase has a specific focus area and difficulty level. Each phase must have a clear feedback mechanism — how does the learner know they succeeded or failed?

  **Structural rules:**

  - **Difficulty progression.** Phases increase in difficulty to keep the learner in deliberate effort (System 2) — learning is building a pattern library, if it's automatic it's not learning. If a phase can be completed on autopilot, it's too easy.
  - **Interleave across sub-topics.** Phases vary in focus to strengthen pattern discrimination — don't repeat the same sub-topic in consecutive phases.
  - **Space the repetition.** Phases revisiting similar sub-topics are spaced apart to consolidate into long-term memory.
  - **Pull-based supporting knowledge.** Supporting disciplines are never separate phases or tracks. They are identified by name and trigger condition — pull knowledge at point of need when practice surfaces a gap.
  - **Feedback mechanism.** Every phase must specify how the learner gets timely, clear feedback in a high-validity environment — where real patterns exist to be learned.

  Synthesize refined themes into a skeleton. For each track include:
  - Track name, scope, and success criteria
  - Phases in order, each with: name, goal (one sentence), focus area, difficulty level relative to prior phase, feedback mechanism
  - Sequencing rationale — why this order, how interleaving and difficulty progression are achieved
  - Pull-based supporting knowledge with name and trigger condition
  - Dependencies between tracks (if multi-track)

  **Self-checks** before presenting:

  1. **Principles check** — does every structural decision serve: transfer-appropriate practice, pull-based knowledge, deliberate effort, interleaving, spacing, or timely feedback?
  2. **Coverage check** — every gap and blind spot in the refined themes is addressed by at least one phase. Nothing fell through the cracks.
  3. **Interleaving check** — no two consecutive phases focus on the same sub-topic.
  4. **Difficulty check** — phases progress in difficulty. No phase is easier than the one before it unless it introduces a new sub-topic.
  5. **Feedback check** — every phase has a specified feedback mechanism.

  If any check fails, fix the skeleton before presenting.

  **Exit criteria:** All self-checks pass. Present the skeleton to the user. Block on explicit user approval before proceeding to curation. The user may iterate on the skeleton.

  ## Step 4: Curation

  **Purpose:** Fill the approved skeleton with resources. No structural changes — the skeleton from Step 3 is the contract.

  **Process:**

  Generate the plan by populating the approved skeleton with resources. Do not ask for confirmation before writing — the user has already approved the structure. They can iterate after. If curation surfaces a structural concern, note it in the plan output under a dedicated section — do not stop or backtrack.

  **Resource Taxonomy** — a classification of learning resources by their relationship to practice.

  1. **Structured practice** — walks you through doing the activity. Courses with exercises, project-based books, guided labs. Remove the doing and the resource collapses.
  2. **Drill** — pool of problems to practice on. Teaching material exists to set up the problems, not the other way around.
  3. **Reference** — supports non-sequential access. Pulled when practice surfaces a gap. Includes documentation, cookbooks, best-practices guides.
  4. **Textbook** — sequential instruction. Later material builds on earlier material. Used as driver when no structured practice resource exists.
  5. **Philosophy** — primary value is a mental model or worldview shift. Always pull-based.

  **Curation rules:**

  1. **Maximize return on time invested.** Every resource must justify its time cost. If a shorter or more focused resource teaches the same concept, use it.
  2. **One driver per phase, everything else is pulled.** Each phase has exactly one resource that sets the structure. Every other resource is pull-based — opened when needed, not scheduled alongside.
  3. **Prefer structured practice as driver.** Always prefer a structured practice resource as the driver if one exists — the learner does the actual activity, not reads about it. If none exists, use the best available resource.

  **Plan sections:**

  1. Purpose & Goals
  2. Learning Model — the six principles, included verbatim so the plan can be reviewed independently.
  3. Structural Rules — the track/phase hierarchy and the rules governing it, included verbatim so the plan carries its own evaluation criteria.
  4. Curation Rules — the three curation rules, included verbatim.
  5. Track and phases
  6. Current Baseline (marked provisional for unbounded, with reassessment triggers)
  7. Scope — in scope / out of scope
  8. Blind spots carried forward from Step 2

  **Resource format** — each resource must include:

  - **Name** — title and author/source
  - **Medium** — book, course, video, docs, etc.
  - **Why it fits** — tied to the user's goals and baseline. No "this is popular" justifications.
  - **Where in the sequence** — which track, which phase, order within phase
  - **Type** — one of: structured practice, drill, reference, textbook, philosophy
  - **Role** — driver or pull-based. Exactly one resource per phase is the driver.

  **Exit criteria:** A complete plan covering all sections, with every resource justified and sequenced. The plan matches the approved skeleton — same track, phases, and sequencing.

  ## Step 5: Review

  **Purpose:** The agent audits the complete plan from Step 4. The plan contains its own evaluation criteria — Learning Model, structural rules, curation rules — so review works against the plan itself.

  **Guiding principles:**

  - Every check must show evidence — a passing check needs reasoning, not just "looks good."
  - Fix resource-level issues in place (swap a resource, change a role, adjust a medium). The plan that exits review should be final.
  - Flag structural concerns but do not act on them — the user will address structure through iteration.

  **Process:**

  Run these checks against the plan:

  1. **Skeleton fidelity check** — does the plan match its own approved skeleton? Same track, phases, and sequencing. No additions, removals, or reclassifications.
  2. **Alignment check** — does the plan actually address the goals, baseline, and constraints from the evaluation? Are there gaps between what was discussed and what was planned?
  3. **Principles check** — does each phase's driver have the learner doing the actual activity, not reading about it? Is the learner kept in deliberate effort (System 2)? Is knowledge pulled at point of need, not pre-taught? Are phases interleaved and spaced?
  4. **Curation rules check** — does every resource maximize return on time invested? Is there exactly one driver per phase? Is a structured practice resource used as driver wherever one exists?
  5. **Interleaving check** — do phases vary in focus across sub-topics? If consecutive phases target the same sub-topic, fix it.
  6. **Medium check** — for each resource, would a different medium teach the same concept faster or more effectively? Consider every medium — video, text, hands-on.
  7. **Resource verification** — for every curated resource, verify it exists and is accurately described using `fetch_url`. If it can't be verified, find a verified replacement. If no replacement, flag the gap explicitly.
  8. **Type check** — is each resource's Type label accurate per the Resource Taxonomy? Would a structured practice resource collapse without the doing? Does a drill lose its value without the problems?

  **Exit criteria:** All checks pass. Every resource verified. No unresolved gaps between evaluation and plan.

  ## Step 6: Output

  **Purpose:** Write the plan to a file.

  **Process:**

  - Save the plan to the current working directory.
  - File name format: `{goal}-learning-plan.md`

  **Exit criteria:** File written and path confirmed to the user.
''
