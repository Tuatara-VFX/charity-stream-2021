using UnityEditor;
using UnityEngine;

[CustomEditor(typeof(VFXSDFSetup))]
public class VFXSDFSetupCustomDrawer : Editor
{
    public override void OnInspectorGUI()
    {
        base.OnInspectorGUI();

        var setup = (VFXSDFSetup)target;

        EditorGUI.BeginDisabledGroup(setup.data == null);
        if (GUILayout.Button("Apply Volume Transform"))
        {
            setup.ApplyVolumeTransform();
        }
        EditorGUI.EndDisabledGroup();
    }
}