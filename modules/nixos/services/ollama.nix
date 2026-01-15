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
      host = config.customGlobal.localHostIPv4;
      port = 11434;
      openFirewall = false;


      # In this configuration I strive to have three classes of LLM, which can be
      # distinguished based on scope of context and degree of autonamy.
      #
      # Level 1: General purpose(prompt model)
      # ----------------------------------------
      #
      # Purpose:
      #  Interactive chat, explanation, brainstorming, and general reasoning.
      #
      # Characteristics:
      # - No repository or file awareness.
      # - Stateless by default(memory only exist if tool adds it to the context).
      # - Responds only to the provide prompt and conversation history.
      #
      # Level 2: Context scoped model(file-scoped / "half agent")
      # -----------------------------------------------------------
      #
      # Purpose:
      #  Editing and refactoring code within explicitly provided files.
      #
      # Characteristics:
      # - Excellent at modifying code that is passed into the context(file by the user).
      # - Supports iterative refinment of the same file.
      # - Does NOT explore repository autonomously.
      # - Depends on the user or the toll to select for it which files are relevant..
      #
      # Important:
      #  This is not an agnet, the model decides how to edit code, but not what to inspect
      #  or where to look.
      #
      # Level 3: Agent
      # ----------------------------------------
      #
      # Purpose:
      #  Multi step tasks across a repository.
      #
      # Characteristics:
      # - Operates inside an execution loop(plan -> act -> observe -> repeat).
      # - uses tools(search, open files, apply pathces).
      # - Maintains state across steps via the controlling tool.
      # - Does not load the entire repository, instead decides what to inspect.
      loadModels = [
        config.customGlobal.AIDefaultModels.prompt  # level 1.
        config.customGlobal.AIDefaultModels.fileScoped   # level 1.
        config.customGlobal.AIDefaultModels.agent # level 1.
        # "devstral-small-2"   # level 3 - needs newer version of ollama.
      ];

      syncModels = true;
    };
  };
}

