import collections
from dataclasses import dataclass
from pathlib import Path
from typing import Tuple, List


@dataclass
class Chemical:
    name: str
    amount: int


@dataclass
class Reaction:
    lhs: List[Chemical]
    rhs: Chemical

    def __init__(self, desc):
        def parse_chemical(desc):
            amount, name = desc.split()
            return Chemical(name=name, amount=int(amount))

        lhs_full, rhs = desc.split(' => ')
        self.lhs = list(map(parse_chemical, lhs_full.split(', ')))
        self.rhs = parse_chemical(rhs)


def parse_input(input):
    return [Reaction(line.strip()) for line in input.strip().split('\n')]


def topological_sort(reactions):
    unsorted_indexes = set(range(len(reactions)))
    result = []
    while unsorted_indexes:
        unsorted_lhs = set(ch.name for idx in unsorted_indexes for ch in reactions[idx].lhs)
        for idx in unsorted_indexes:
            chemical = reactions[idx].rhs.name
            if chemical not in unsorted_lhs:
                unsorted_indexes.remove(idx)
                result.append(chemical)
                break
    return result


def get_fuel_cost(reactions, amount):
    order = topological_sort(reactions)
    target = order[0]
    assert target == 'FUEL'
    needed = collections.defaultdict(int, {target: amount})
    target_to_reaction = {r.rhs.name: r for r in reactions}
    for chemical in order:
        if (amount := needed.get(chemical)) is not None:
            cur_reaction = target_to_reaction[chemical]
            multiplier = (amount + cur_reaction.rhs.amount - 1) // cur_reaction.rhs.amount
            for prerequisite in cur_reaction.lhs:
                needed[prerequisite.name] += multiplier * prerequisite.amount
    return needed['ORE']


def solve_easy(reactions):
    return get_fuel_cost(reactions, 1)


def solve_hard(reactions):
    left, right = 1, 10 ** 9
    while left < right:
        mid = (left + right + 1) // 2
        if get_fuel_cost(reactions, mid) <= 10 ** 12:
            left = mid
        else:
            right = mid - 1
    return left


def test_sample():
    samples = list(map(parse_input, [
        '''
        10 ORE => 10 A
        1 ORE => 1 B
        7 A, 1 B => 1 C
        7 A, 1 C => 1 D
        7 A, 1 D => 1 E
        7 A, 1 E => 1 FUEL
        ''',
        '''
        9 ORE => 2 A
        8 ORE => 3 B
        7 ORE => 5 C
        3 A, 4 B => 1 AB
        5 B, 7 C => 1 BC
        4 C, 1 A => 1 CA
        2 AB, 3 BC, 4 CA => 1 FUEL
        ''',
        '''
        157 ORE => 5 NZVS
        165 ORE => 6 DCFZ
        44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
        12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
        179 ORE => 7 PSHF
        177 ORE => 5 HKGWZ
        7 DCFZ, 7 PSHF => 2 XJWVT
        165 ORE => 2 GPVTF
        3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
        ''',
        '''
        2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
        17 NVRVD, 3 JNWZP => 8 VPVL
        53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
        22 VJHF, 37 MNCFX => 5 FWMGM
        139 ORE => 4 NVRVD
        144 ORE => 7 JNWZP
        5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
        5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
        145 ORE => 6 MNCFX
        1 NVRVD => 8 CXFTF
        1 VJHF, 6 MNCFX => 4 RFSQX
        176 ORE => 6 VJHF
        ''',
        '''
        171 ORE => 8 CNZTR
        7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
        114 ORE => 4 BHXH
        14 VRPVC => 6 BMBT
        6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
        6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
        15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
        13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
        5 BMBT => 4 WPTQ
        189 ORE => 9 KTJDG
        1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
        12 VRPVC, 27 CNZTR => 2 XDBXC
        15 KTJDG, 12 BHXH => 5 XCVML
        3 BHXH, 2 VRPVC => 7 MZWV
        121 ORE => 7 VRPVC
        7 XCVML => 6 RJRHP
        5 BHXH, 4 VRPVC => 5 LTCX
        '''
    ]))
    for s in samples:
        print(solve_easy(s))
    for s in samples[2:]:
        print(solve_hard(s))


if __name__ == '__main__':
    data = parse_input(Path('14.txt').read_text())
    print(solve_easy(data))
    print(solve_hard(data))
