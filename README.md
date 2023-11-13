# cooklang

Ideas:

- Generate (part of) a site for a recipes folder
  - support search
  - support inline timers
  - integrate with groceries list
- Integrate with groceries list in todo.txt format
- Meta-info
  - soorten ingredienten
  - eenheden
  - domeinen waar recepten vandaan komen

# Design

## geintegreerd of niet?

- als geintegreerd: web-server nodig
- als geintegreerd: embedden extra ingredienten paginas lastig
- als geintegreerd: boodschappen om ai-banana, recepten op scd.his.ke lastig

Mogelijkheid:
- ondersteun geintegreerde server met boodschappen support of static HTML export
- losse componenten:
  - receptenboek met support for todo-list API export
  - boodschappen-server
  maar wie brengt dan eenheden samen?

Voor samenwerking is monolithisch project denk ik beter. CLI met:

- serve commando voor webserver
- generate command voor static receptenboek html

# Stappen

1. Genereer HTML en vervang scd.his.ke
2. Recepten integratie en vervang groceries v2
3. Extra functionaliteit
