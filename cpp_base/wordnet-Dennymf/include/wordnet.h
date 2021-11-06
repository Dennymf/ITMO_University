#pragma once

#include "word.h"

#include <algorithm>
#include <set>
#include <queue>
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class Digraph {
public:
    using adjacency_type = std::vector<unsigned>;

    void set_size(const unsigned size);

    void add_edge(const unsigned from, const unsigned to);

    std::size_t size() const;

    const adjacency_type &operator[](const unsigned index) const;

private:
    std::vector<adjacency_type> edges;
};

class ShortestCommonAncestor {
    const Digraph &graph;

    std::vector<int> bfs(const unsigned begin) const;

    std::tuple<unsigned, unsigned> calculate_ancestor(const unsigned u, const unsigned v) const;

    std::tuple<unsigned, unsigned> calculate_ancestor_subset(
            const std::vector<unsigned> &subset_a, const std::vector<unsigned> &subset_b) const;

public:
    explicit ShortestCommonAncestor(const Digraph &graph);

    int ancestor_subset(const std::vector<unsigned> &subset_a, const std::vector<unsigned> &subset_b) const;

    int length_subset(const std::vector<unsigned> &subset_a, const std::vector<unsigned> &subset_b) const;
};

class WordNet {
    std::unordered_set<word> words;
    std::unordered_map<word, std::vector<unsigned>> word_to_indexes;
    std::unordered_map<unsigned, word> index_to_gloss;
    Digraph graph;

    void load_synsets(const std::string &path);

    void load_hypernyms(const std::string &path);

public:
    WordNet(const std::string &synsets_fn, const std::string &hypernyms_fn);

    using Iterator = std::unordered_set<word>::iterator;

    Iterator nouns();

    Iterator end();

    bool is_noun(const std::string &word) const;

    std::string sca(const std::string &noun1, const std::string &noun2) const;

    int distance(const std::string &noun1, const std::string &noun2) const;
};

class Outcast {
    const WordNet &wordnet;

public:
    explicit Outcast(const WordNet &wordnet);

    std::string outcast(const std::vector<std::string> &nouns);
};