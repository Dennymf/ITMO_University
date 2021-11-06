#include "wordnet.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <tuple>
#include <vector>

namespace {
    template<typename T, typename S>
    std::vector<T> split(const T &container, const S &separator) {
        std::vector<T> result;
        for (auto from = container.begin();;) {
            const auto to = std::find(from, container.end(), separator);
            result.emplace_back(T(from, to));
            if (to == container.end()) {
                return result;
            }
            from = std::next(to);
        }
    }

    std::tuple<unsigned, std::vector<std::string>, std::string> parse_synsets_line(const std::string &line) {
        auto columns = split(line, ',');
        const auto index = std::stoi(columns[0]);
        const auto synonyms = split(columns[1], ' ');
        const auto gloss = columns[2];
        return {index, synonyms, gloss};
    }

    std::tuple<unsigned, std::vector<unsigned>> parse_hypernyms_line(const std::string &line) {
        auto columns = split(line, ',');
        const auto index = stoi(columns[0]);
        std::vector<unsigned> words(columns.size() - 1);
        for (unsigned i = 1; i < columns.size(); i++) {
            words[i - 1] = stoi(columns[i]);
        }
        return {index, words};
    }
}

void Digraph::set_size(const unsigned size) {
    this->edges.resize(size);
}

void Digraph::add_edge(const unsigned from, const unsigned to) {
    this->edges[from].emplace_back(to);
}

std::size_t Digraph::size() const {
    return edges.size();
}

const Digraph::adjacency_type &Digraph::operator[](const unsigned index) const {
    return edges[index];
}

void WordNet::load_synsets(const std::string &path) {
    std::ifstream is(path);
    for (std::string line; std::getline(is, line);) {
        const auto&[index, synonyms, gloss] = parse_synsets_line(line);
        index_to_gloss[index] = gloss;
        for (const auto &word : synonyms) {
            words.emplace(word);
            word_to_indexes[word].emplace_back(index);
        }
    }
    is.close();
}

void WordNet::load_hypernyms(const std::string &path) {
    std::ifstream is(path);
    this->graph.set_size(index_to_gloss.size());
    for (std::string line; std::getline(is, line);) {
        const auto &[from, to] = parse_hypernyms_line(line);
        for (const auto &word : to) {
            this->graph.add_edge(from, word);
        }
    }
    is.close();
}

WordNet::WordNet(const std::string &synsets_fn, const std::string &hypernyms_fn) {
    load_synsets(synsets_fn);
    load_hypernyms(hypernyms_fn);
}

WordNet::Iterator WordNet::nouns() {
    return words.begin();
}

WordNet::Iterator WordNet::end() {
    return words.end();
}

bool WordNet::is_noun(const std::string &word) const {
    return words.find(word) != words.end();
}

std::string WordNet::sca(const std::string &noun1, const std::string &noun2) const {
    auto first = word_to_indexes.at(noun1);
    auto second = word_to_indexes.at(noun2);
    ShortestCommonAncestor shortest_common_ancestor(graph);
    return index_to_gloss.at(shortest_common_ancestor.ancestor_subset(first, second));
}

int WordNet::distance(const std::string &noun1, const std::string &noun2) const {
    auto first = word_to_indexes.at(noun1);
    auto second = word_to_indexes.at(noun2);
    ShortestCommonAncestor shortest_common_ancestor(graph);
    return shortest_common_ancestor.length_subset(first, second);
}

std::vector<int> ShortestCommonAncestor::bfs(const unsigned begin) const {
    std::vector<int> distance(graph.size(), -1);
    std::queue<unsigned> q({begin});
    distance[begin] = 0;
    while (!q.empty()) {
        const auto &v = q.front();
        q.pop();
        for (const unsigned &u : graph[v]) {
            if (distance[u] == -1) {
                distance[u] = distance[v] + 1;
                q.push(u);
            }
        }
    }
    return distance;
}

std::tuple<unsigned, unsigned> ShortestCommonAncestor::calculate_ancestor(const unsigned u, const unsigned v) const {
    const auto first = bfs(u);
    const auto second = bfs(v);
    unsigned min_length = -1;
    unsigned id = -1;
    for (unsigned i = 0; i < first.size(); i++) {
        if (first[i] != -1 && second[i] != -1 && static_cast<unsigned>(first[i] + second[i]) < min_length) {
            min_length = first[i] + second[i];
            id = i;
        }
    }
    return {id, min_length};
}

std::tuple<unsigned, unsigned> ShortestCommonAncestor::calculate_ancestor_subset(
        const std::vector<unsigned> &subset_a, const std::vector<unsigned> &subset_b) const {
    unsigned min_length = -1;
    unsigned id = -1;
    for (const auto &x : subset_a) {
        for (const auto &y : subset_b) {
            const auto&[anc_id, anc_len] = calculate_ancestor(x, y);
            if (anc_len < min_length) {
                min_length = anc_len;
                id = anc_id;
            }
        }
    }
    return {id, min_length};
}

ShortestCommonAncestor::ShortestCommonAncestor(const Digraph &graph) : graph(graph) {}

int ShortestCommonAncestor::ancestor_subset(const std::vector<unsigned> &subset_a,
                                            const std::vector<unsigned> &subset_b) const {
    return static_cast<int>(std::get<0>(calculate_ancestor_subset(subset_a, subset_b)));
}

int ShortestCommonAncestor::length_subset(const std::vector<unsigned> &subset_a,
                                          const std::vector<unsigned> &subset_b) const {
    return static_cast<int>(std::get<1>(calculate_ancestor_subset(subset_a, subset_b)));
}

Outcast::Outcast(const WordNet &wordnet) : wordnet(wordnet) {}

std::string Outcast::outcast(const std::vector<std::string> &nouns) {
    if (nouns.size() <= 2) {
        return "";
    }
    std::vector<std::vector<unsigned>> distance(nouns.size(), std::vector<unsigned>(nouns.size()));
    for (unsigned i = 0; i < nouns.size(); i++) {
        for (unsigned j = i + 1; j < nouns.size(); j++) {
            distance[i][j] = distance[j][i] = wordnet.distance(nouns[i], nouns[j]);
        }
    }
    unsigned max_sum = 0;
    unsigned id = 0;
    for (unsigned i = 0; i < distance.size(); i++) {
        auto sum = std::accumulate(distance[i].begin(), distance[i].end(), static_cast<unsigned>(0));
        if (sum > max_sum) {
            max_sum = sum;
            id = i;
        }
    }
    return nouns[id];
}