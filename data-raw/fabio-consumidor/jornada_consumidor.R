# jornada completa --------------------------------------------------------

DiagrammeR::grViz("
  digraph boxes_and_circles {

    # a 'graph' statement
    graph [overlap = false, fontsize = 10, rankdir = LR]


    # several 'node' statements
    node [shape = box, color = '#26C485', style = filled, fontcolor = '#233262']
      sac [label = 'SAC'];
      ouvidoria [label = 'Ouvidoria'];

    node [shape = box, color = '#ADC910', style = filled, fontcolor = '#233262']
      procon1 [label = 'Procon /\nConsumidor.gov']
      procon2 [label = 'Procon /\nConsumidor.gov']
      procon3 [label = 'Procon /\nConsumidor.gov']

    node [shape = box, color = '#233262', style = filled, fontcolor = 'White']
      jud1 [label = 'Judiciário']
      jud2 [label = 'Judiciário']
      jud3 [label = 'Judiciário']
      jud4 [label = 'Judiciário']

    # several 'edge' statements
    sac->ouvidoria sac->procon2 sac->jud2 procon1->jud2 jud1
    ouvidoria->procon3 procon2->jud3
    procon3->jud4

  }
  ")

# jornada externo-judicial --------------------------------------------------------

DiagrammeR::grViz("
  digraph boxes_and_circles {

    # a 'graph' statement
    graph [overlap = false, fontsize = 10, rankdir = LR]


    # several 'node' statements

    node [shape = box, color = '#ADC910', style = filled, fontcolor = '#233262']
      procon [label = 'Procon /\nConsumidor.gov']

    node [shape = box, color = '#233262', style = filled, fontcolor = 'White']
      jud1 [label = 'Judiciário']
      jud2 [label = 'Judiciário']

    # several 'edge' statements
    procon->jud2 jud1
  }
  ")
