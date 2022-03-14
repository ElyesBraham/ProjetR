# ProjetR
Bonjour ! Voici une brève description de notre projet R de l’UE HAX815X
Ce projet a pour but de générer aléatoirement des grilles de Sudoku complètes  et incomplètes associées à un niveau de difficulté , ainsi que de résoudre des grilles de Sudoku incomplètes 

__authors__ = ("Nina MARTHE", "Elyes BRAHAM")

__contact__ = ("nina.marthe@etu.umontpellier.fr","elyes.braham@etu.umontpellier.fr")

__version__ = "0.0.1"

__date__ = "14/03/2022"

__licence__ ="This program is free software: you can redistribute it and/or modify"
        under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.
        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
        GNU General Public License for more details.
        You should have received a copy of the GNU General Public License
        along with this program. If not, see <https://www.gnu.org/licenses/>.

L'objectif est de remplir une grille 9x9 avec des chiffres  de 1 à 9 de sorte que chaque colonne, chaque ligne et diagonale des 9 sous-grilles de 3x3 qui composent la grille.
Installer le package :
library(devtools)
devtools::install_github("NinaMarthe/ProjetR")
