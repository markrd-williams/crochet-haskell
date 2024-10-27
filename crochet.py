import igl
import numpy as np 
import os
from meshplot import plot
import copy
import re

def read_off(path):
    with open(path, "r") as file:
        lines = (line.strip() for line in file)
        lines = list((re.split(r'\s+', line) for line in lines if line and line[0] != "#"))
        if 'OFF' != lines[0][0]:
            raise('Not a valid OFF header')
        n_verts, n_faces, n_dontknow = tuple([int(s) for s in lines[1]])
        V = np.array([[float(s) for s in lines[i_vert]] for i_vert in range(2,n_verts+2)])
        F = np.array([[int(s) for s in lines[i_face]][1:] for i_face in range(n_verts+2,n_verts+n_faces+2)])
        return V, F



def next_row_crochet(current_stitches, goal_stitches):
    if goal_stitches == 0:
        return (
            f'Finish by joining.'
        )

    if goal_stitches >= current_stitches:
        stitch_per_hole = goal_stitches // current_stitches
        extra_stitches = goal_stitches % current_stitches
        if extra_stitches != 0:
            extra_stitch_spacing = current_stitches // extra_stitches
            extra_ending = goal_stitches % extra_stitches
            if stitch_per_hole >= 1:
                return (
                    f'Seq {extra_stitches}:\n'
                    f'  Seq {extra_stitch_spacing - 1}:\n'
                    f'      SC + Increase {stitch_per_hole - 1} times\n'
                    f'  SC + Increase {stitch_per_hole}\n'
                    f'Seq {extra_ending}:\n'
                    f'  SC + Increase {stitch_per_hole}\n'
                )
            else:
                return (
                    f'Seq {extra_stitches}:\n'
                    f'  Seq {extra_stitch_spacing - 1}:\n'
                    f'      SC\n'
                    f'  SC + Increase\n'
                    f'Seq {extra_ending}:\n'
                    f'  SC\n'
                )
        else:
            if stitch_per_hole > 1:
                return (
                    f'Seq {current_stitches}:\n'
                    f'  SC + Increase {stitch_per_hole - 1} times\n'
                )
            else:
                return (
                    f'Seq {current_stitches}:\n'
                    f'  SC'
                )

    decrease_per_hole = current_stitches // goal_stitches - 1
    extra_decreases = current_stitches % goal_stitches
    if extra_decreases != 0:
        extra_spacing = goal_stitches // extra_decreases
        extra_ending = goal_stitches % extra_decreases
        if decrease_per_hole >= 1 and extra_ending > 0:
            return (
                f'Seq {extra_decreases}:\n'
                f'  Seq {extra_spacing - 1}:\n'
                f'      Skip {decrease_per_hole} stitches\n'
                f'      SC\n'
                f'  Skip {decrease_per_hole + 1} stitches\n'
                f'  SC\n'
                f'Skip {extra_ending}\n'
                f'SC\n'
            )
        elif decrease_per_hole >= 1 and extra_ending == 0:
            return (
                f'Seq {extra_decreases}:\n'
                f'  Seq {extra_spacing - 1}:\n'
                f'      Skip {decrease_per_hole} stitches\n'
                f'      SC\n'
                f'  Skip {decrease_per_hole + 1} stitches\n'
                f'  SC\n'
            )
        elif decrease_per_hole == 0 and extra_ending > 0: 
            return (
                f'Seq {extra_decreases}:\n'
                f'  Seq {extra_spacing - 1}:\n'
                f'      SC\n'
                f'  Skip 1 stitch\n'
                f'  SC\n'
                f'Skip {extra_ending}\n'
                f'SC\n'
            )
        else:
            return (
                f'Seq {extra_decreases}:\n'
                f'  Seq {extra_spacing - 1}:\n'
                f'      SC\n'
                f'  Skip 1 stitch\n'
                f'  SC\n'
            )
    return (
        f'Seq {goal_stitches}:\n'
        f'  Skip {decrease_per_hole}\n'
        f'  SC\n'
    )


def neighbours(V: np.array, F: np.array): 
    N = {}
    for i in range(V.shape[0]):
        N[i] = set()
    for (i,j,k) in F:
        if (i == 1 or j == 1 or k == 1):
            print(f'i: {i}')
            print(f'j: {j}')
            print(f'k: {k}')
        N[i].add(j)
        N[i].add(k)
        N[j].add(i)
        N[j].add(k)
        N[k].add(i)
        N[k].add(j)
    return N

def outer_boundary(bdry, S: set, N):
    return {n for i in bdry for n in N[i] if not n in S}


V, F = read_off(os.path.join(os.getcwd(),"data", "untitled.off"))
K = igl.gaussian_curvature(V, F)
N = neighbours(V, F)

cur_stitches = len(N[1])
instructions = "Slipknot\n2x CH\n" + str(cur_stitches) + " SC in first chain\n"
tot = copy.deepcopy(N[1])
tot.add(1)
boundary = copy.deepcopy(N[1])


while boundary:
    next_row = {n for i in boundary for n in N[i] if not n in tot}
    goal_stitches = len(next_row)
    
    instructions += next_row_crochet(cur_stitches, goal_stitches)
    
    tot = tot.union(next_row)
    boundary = next_row
    cur_stitches = len(next_row)

print(instructions)
