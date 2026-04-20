{ config, lib, ... }:
let
  cfg = config.services.ollama;
in
{
  config = lib.mkIf cfg.enable {

    # ollama is the manager for local LLMs.
    # - Responsible for downloading and running models locally
    # - Runs locally an interface with the models(GPU/CPU) and exposes API via HTTP.
    #   Tools like aichat and adir connect to that API to provide workflows.
    services.ollama = {
      host = config.custom.shared.localHostIPv4;
      port = 11434;
      openFirewall = false;


      # Model size memory and speed
      # ---------------------------
      # Every model is loaded into memory, be it to RAM or VRAM, the calculation is
      # straight forward:
      # model memory usage = [number of model parameters ] x [ bits per parameter ] / 8
      #
      # Although models come in different sizes some of them are reduced using
      # quantization which reduces the number of bits per a single parameter.
      # This shrinks the model size at the cost of precision.
      # fp16(16 bit) → Q8(8 bit) → Q4(4.5 bit) → Q2(2.5 bit)
      # Q4 is the default sweet spot: this shrink will do negligible quality loss
      # for most tasks. Going lower than Q4 shows measurable degradation in quality.
      #
      # So when going with Q4:
      # | model parameter count | size in memory |
      # | 4B                    | ~3GB           |
      # | 9B                    | ~5.5GB         |
      # | 14B                   | ~8.5GB         |
      # | 32B                   |~18.5GB         |
      #
      # From prompt to response
      #
      # Input: is the string you ask "What is the weather tomorrow", it gets
      # split into tokens(which are chunks of text, usually smaller than words but
      # sometimes whole words), as the model operates in tokens only.
      # All of them go via the entire model weights(which is represented by layers)
      #
      # meaning the CPU(for RAM) and GPU(for VRAM) need to read the entire model size
      # in memory in order to do calculation on it.
      #
      # Output: is where things get more interesting. The model produces token 1 of the
      # response for which it does full pass through all the layers, meaning the entire
      # memory is being read by CPU/GPU just to produce a single token. Then it takes
      # that first response token appends it to the response context and does another
      # full pass through all the layers yet again the entire model size gets read.
      # meaning for 50 token output is 50 full read passes over the entire model
      # size.
      #
      # A simple conclusion can be drawn, the bigger the model and the more tokens need
      # to be generated(be it for a single response or a multi step action) the more
      # data will need to be read. A simple calculation will be:
      # [ data to read ] = [ (number of output tokens) + 1 ] x [ size of model in memory ]
      #
      # And this is where memory speed comes into play. Slow memory is fine for short
      # responses(a few hundred tokens), but becomes a real bottleneck as output length
      # or model size grows — the wait compounds with every token generated.

      # RAM vs VRAM
      # ----------
      # Both can be used to store the same model and to produce identical output
      # VRAM is just faster when compared to DDR4/5.
      # Both will produce tokens but VRAM will do it faster.
      #
      # And faster is important when it comes to scale.
      #
      # For a 3GB model generating one token:
      # - On GPU (936 GB/s): 3GB / 936 = ~3ms per token → ~300 tokens/sec theoretical max
      # - On CPU via DDR5 (70 GB/s): 3GB / 70 = ~43ms per token → ~23 tokens/sec theoretical max
      # - On CPU via DDR4 (40 GB/s): 3GB / 40 = ~75ms per token → ~13 tokens/sec theoretical m
      #
      # And that is the entire story. The math (multiplications) takes negligible
      # time compared to waiting for the data to arrive. But given a big enough
      # model, even a powerful GPU will start choking.

      # Given the above, model choice depends on what you're optimizing for:
      #
      # Model selection through the lens of utility
      # ------------------------------------
      #
      # Models can be evaluated on 3 axes
      #
      # 1. Parameter count(weights):
      #    More parameters, means bigger capacity to represent distinctions.
      #    Meaning bigger models are more reliable when presented with ambiguity.
      #    More specifically ability to understand poorly defined / vague prompts.
      #    This also means that small models will perform poorly on multi step tasks.
      #
      #    Note: Size doesn't fix bad training, Size amplifies the training put into the model.
      #
      # 2. Training data:
      #    What model has learned - code, medical, general knowledge, etc.
      #    A 4B model trained on curated / refined general knowledge data can
      #    beat a bigger model(E.g: 14B) that is trained on noise.
      #
      # 3. Alignment with harness (tooling):
      #    Whether the model was fine-tuned for chat, code editing or "structured
      #    tool-calling" (agentic use) will determine what "harness" the model will
      #    work in well. Big high quality chat model might fail when asked to perform
      #    multiple step tasks with tools.

      # Based on this evaluation models can be divided into the following categories
      #
      # General purpose(chat / prompt model)
      # ----------------------------------------
      #
      # Purpose:
      #  Interactive chat, explanation, brainstorming, and general reasoning.
      #
      # Characteristics:
      # - No repository or file awareness.
      # - Stateless by default(memory only exists if tool adds it to the context).
      # - Responds only to the provided prompt and conversation history.
      #
      # Refined for single purpose(E.g: Coding)
      # -----------------------------------------------------------
      #
      # Purpose:
      #  Editing and refactoring code within explicitly provided files.
      #
      # Characteristics:
      # - Excellent at modifying code that is passed into the context(file by the user).
      # - Supports iterative refinement of the same file.
      # - Does NOT explore repository autonomously.
      # - Depends on the user or the tool to select for it which files are relevant.
      #
      # Agentic(could be code or general purpose like personal assistant (E.g: openclaw))
      # ----------------------------------------
      #
      # Purpose:
      #  Multi step tasks across a repository while using tools
      #
      # Characteristics:
      # - Operates inside an execution loop(plan -> act -> observe -> repeat).
      # - uses tools(search, open files, apply patches).


      # Hosts own services.ollama.loadModels. Keep syncModels enabled here so the
      # declared host model set stays authoritative, but do not define a shared
      # default model list in this module.
      syncModels = true;
    };
  };
}
