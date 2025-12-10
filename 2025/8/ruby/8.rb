require_relative '../../utils/ruby/utils'
require 'pp'

def dist2(a, b)
  a.zip(b).sum { |x,y| (x - y)**2 }
end

def edges_to_adj_list(l)
  graph = {}
  l.each do |(i, j, d)|
    graph[i].nil? ? graph[i] = [j] : graph[i] << j
    graph[j].nil? ? graph[j] = [i] : graph[j] << i
  end
  graph
end

def build_ccs(graph)
  cur_component = 0
  components = graph.keys.map { |k| [k, 0] }.to_h
  visited = Set.new

  dfs = Proc.new do |i|
    next if visited.include?(i)

    visited << i
    components[i] = cur_component
    unvisited = graph[i].reject { |j| visited.include?(j) }
    unvisited.each { |j| dfs.call(j) }
  end

  graph.keys.each do |i|
    if !visited.include?(i)
      dfs.call(i)
      cur_component += 1
    end
  end

  components
end

class Day8 < AocSolution
  def part_1(filename, sample: true)
    points = File.readlines(filename, chomp: true).map do |line|
      line.split(',').map(&:to_i)
    end

    distances = {}
    points.each_with_index do |p1, i|
      points.each_with_index do |p2, j|
        next if i >= j

        distances[[i, j]] = dist2(p1, p2)
      end
    end

    sorted_distances = distances
                         .sort_by { |k, v| v }
                         .drop_while { |k, v| v.zero? }
                         .map { |(k, _)| k }

    edges = sorted_distances.take(sample ? 10 : 1000)
    adj_list = edges_to_adj_list(edges)
    components = build_ccs(adj_list)
    components.values.tally.values.sort.reverse.take(3).reduce(&:*)
  end

  def part_2(filename, sample: true)
    points = File.readlines(filename, chomp: true).map do |line|
      line.split(',').map(&:to_i)
    end

    # build distance matrix
    distances = {}
    points.each_with_index do |p1, i|
      points.each_with_index do |p2, j|
        next if i >= j

        distances[[i, j]] = dist2(p1, p2)
      end
    end

    # build graph
    edges = distances.sort_by { |k, v| v }.map { |k, _| k }
    adj_list = {}
    ccs = Array.new(points.length, -1)
    next_component = -1

    last_tally_length = ccs.length
    # for each edge, in order of distance, get adjacent components.
    # if no adjacent components, then break into a new one.
    # otherwise, set all adjacent components to the smallest one.
    # add the edge to the adjacency list
    # break once we've made the last component
    p1, p2 = edges.each_with_index do |(i, j), t|
      adjacent_components = ((adj_list[i]&.map { |ii| ccs[ii] } || []) +
                             (adj_list[j]&.map { |jj| ccs[jj] } || [])).uniq - [-1]

      if adjacent_components == []
        # No adjacent components. Start a new one.
        next_component += 1
        ccs[i] = next_component
        ccs[j] = next_component
      else
        # Adjacent component exists, use the smallest.
        # All adjacent components become min
        use_component = adjacent_components.min
        ccs[i] = use_component
        ccs[j] = use_component
        ccs.size.times do |k|
          if adjacent_components.include?(ccs[k])
            ccs[k] = use_component
          end
        end
      end

      adj_list[i].nil? ? adj_list[i] = [j] : adj_list[i] << j
      adj_list[j].nil? ? adj_list[j] = [i] : adj_list[j] << i

      p [t, ccs.map{ |i| i == -1 ? '_' : i.to_s(36) }.join('')]

      break [i, j] if ccs.tally.length == 1 and last_tally_length != 1

      last_tally_length = ccs.tally.length
    end

    points[p1][0] * points[p2][0]
  end
end

#p Day8.new('../input').run_part(2, sample: true)
p Day8.new('../input').run_part(2, sample: false)
