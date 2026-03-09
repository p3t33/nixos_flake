# Home Assistant NixOS Module — Design Philosophy

## Core Principle: Data and Function are Separate Concerns

### Data (schema.nix)

The schema is organized around the real world — rooms contain devices, devices
have properties. This structure exists purely for **human readability and data
integrity**. It maps to physical reality so the config is easy to read, write,
and reason about.

```
rooms
  office
    plugs
      desk: { switch, power, energy }
  master_bathroom
    plugs
      washing_machine: { switch, power, energy, notifyWhenDone }
```

This organizational structure carries **no obligation to consumers**. It is a
convenient way to write and maintain data. Nothing more.

### Functions (lovelace.nix, automations.nix)

Functional files traverse the entire schema and pick whatever they need at
whatever scope their function requires. They are not bound by how the data is
organized:

```
device-scoped:    notify when washing machine finishes
room-scoped:      turn off all devices when leaving the office
cross-room:       when leaving home, turn off everything everywhere
cross-device:     if any plug draws >2kW AND it's 2am → alert
multi-condition:  washing machine done AND nobody home → delay notification
```

A functional file asks "what data do I need?" not "what device type am I?".
Scope is determined by the function, not by the data's organizational structure.

### Why Per-Device Function Files Are Wrong

Organizing functions by device type (e.g. plugs.nix owns plug automations AND
plug rendering) seems natural at first but breaks down because:

1. **Automations cross device boundaries.** "When washing machine finishes AND
   lights are off → dim instead of notify" involves plugs AND lights. No single
   device file can own it cleanly.

2. **Rendering crosses device boundaries.** An energy dashboard combines plug
   consumption, solar production, and battery state. A room summary card shows
   lights, plugs, and climate together.

3. **You restrict functions by how data is arranged.** Organizing functions
   around the data structure forces every automation and every card to fit into
   a device-type box. As the system grows, cross-device operations become the
   norm, not the exception.

Data is organized for humans. Functions are organized for what they do.

## File Structure

```
schema.nix       — what exists: rooms, device types, device properties
lovelace.nix     — rendering: reads schema, builds dashboard at any scope
automations.nix  — behavior: reads schema, generates automations at any scope
default.nix      — core HA service configuration + imports
```

## Adding a New Device Type (e.g. lights)

1. Add light options to the room submodule in `schema.nix`
2. Add light data to rooms in `schema.nix`
3. Add light card rendering to `lovelace.nix`
4. Add light automations to `automations.nix`

`schema.nix` remains readable because it mirrors the real world.
`lovelace.nix` and `automations.nix` remain coherent because each has one job.
